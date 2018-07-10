﻿// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

module WebSharper.Core.Unpacking

open System
open System.IO
open System.Collections.Generic
open FileSystem
open EmbeddedResourceNames

module PC = WebSharper.PathConventions
module Re = Resources 
module B = Binary
module M = Metadata
module CT = ContentTypes

type ExpressionOptions =
    | FullMetadata
    | DiscardExpressions
    | DiscardInlineExpressions
    | DiscardNotInlineExpressions

type RuntimeHeader =
    {
        DownloadResources : bool
        SettingsUsed : (string * option<string>)[]
        UseSourceMap : bool
        SourceAssemblies : (string * int64)[]
        ExpressionOptions : ExpressionOptions
    }

let UnpackedMetadataEncoding =
    try
        M.IO.EncodingProvider.DeriveEncodingWithHeader(typeof<RuntimeHeader>, typeof<M.Info>, M.IO.CurrentVersion)
    with B.NoEncodingException t ->
        failwithf "Failed to create binary encoder for type %s" t.FullName

let IsWebResourceAttribute (fullName: string) =
    fullName = "System.Web.UI.WebResourceAttribute"
    || fullName = "WebSharper.WebResourceAttribute"

let (|StringArg|_|) (attr: System.Reflection.CustomAttributeTypedArgument) =
    if attr.ArgumentType = typeof<string> then
        Some (attr.Value :?> string)
    else
        None

type IAssembly =
    abstract member Name : string
    abstract member GetResourceStream : string -> option<Stream>
    abstract member TimeStamp : int64
    abstract member WebResources : seq<string * string>
    abstract member DownloadableResources : seq<Re.IDownloadableResource>

type RuntimeAssembly (asm: Reflection.Assembly) =
    interface IAssembly with
        member this.Name = asm.FullName
        member this.GetResourceStream (name: string) = 
            match asm.GetManifestResourceStream name with
            | null ->
                let shortName = asm.FullName.Split(',').[0] 
                asm.GetManifestResourceStream (shortName + "." + name) |> Option.ofObj
            | s -> Some s 

        member this.TimeStamp =
            File.GetLastWriteTimeUtc(asm.Location).ToFileTimeUtc()
        member this.WebResources =
            asm.CustomAttributes
            |> Seq.choose (fun attr ->
                if IsWebResourceAttribute attr.AttributeType.FullName then
                    match Seq.toList attr.ConstructorArguments with
                    | [StringArg resourceName; StringArg contentType] ->
                        Some (resourceName, contentType)
                    | _ -> None
                else None
            )

        member this.DownloadableResources =
            let localResTyp = typeof<Re.IDownloadableResource>
            asm.GetTypes() |> Seq.choose (fun t ->
                if t.GetInterfaces() |> Array.contains localResTyp then
                    try 
                        System.Activator.CreateInstance(t) :?> Re.IDownloadableResource |> Some
                    with _ -> None
                else None
            )
let Unpack
    (assemblies: seq<IAssembly>) (wsRuntimePath: string) (compMetas: option<seq<M.Info>>)
    (rootDirectory: string) (getSetting: string -> option<string>) =
    
    let download =
        match getSetting SettingKeys.UseDownloadedResources with
        | None -> false
        | Some s ->
            match bool.TryParse(s) with
            | true, v -> v
            | _ -> false

    let sourceMap =
        match getSetting SettingKeys.WebSharperSourceMaps with
        | None -> false
        | Some s ->
            match bool.TryParse(s) with
            | true, v -> v
            | _ -> false

    let filterExpressions =
        match getSetting SettingKeys.WebSharperSharedMetadata |> Option.map (fun s -> s.ToLower()) with
        | Some "inlines" -> DiscardNotInlineExpressions
        | Some "notinlines" -> DiscardInlineExpressions
        | Some "full" | None -> FullMetadata
        | _ -> DiscardExpressions

    let assemblies = List.ofSeq assemblies

    let failIfHeaderNotUpToDate (h: obj) =
        match h with
        | :? RuntimeHeader as header ->
            if download && not header.DownloadResources then failwith "Downloading external resources was turned on since latest unpack"        
            if sourceMap && not header.UseSourceMap then failwith "Source mapping was turned on since latest unpack"        
            if not sourceMap && header.UseSourceMap then failwith "Source mapping was turned off since latest unpack"     
            if filterExpressions <> header.ExpressionOptions then failwith "Expression kinds to keep in runtime metadata was changed since last unpack"     
            let asms = Dictionary() 
            for a in assemblies do 
                asms.Add(a.Name, a) |> ignore
            for (a, ts) in header.SourceAssemblies do
                match asms.TryGetValue(a) with
                | true, asm ->
                    if asm.TimeStamp <> ts then
                        failwithf "Assembly %s has different timestamp than latest unpack" a 
                    asms.Remove(a) |> ignore
                | _ ->
                    failwithf "Assembly %s was removed since latest unpack" a 
            if asms.Count > 0 then
                let a = Seq.head asms   
                failwithf "Assembly %s was added since latest unpack" a.Key 

        | _ -> failwith "Could not read header from wsruntime file" 

    let fromFile =
        if File.Exists wsRuntimePath then
            try 
                use stream = File.OpenRead wsRuntimePath 
                UnpackedMetadataEncoding.Decode(stream, headerCont = failIfHeaderNotUpToDate) :?> M.Info |> Some   
            with e ->
                None
        else None

    match fromFile with 
    | Some meta -> 
        meta, DependencyGraph.Graph.FromData(meta.Dependencies), []
    | _ ->

        if File.Exists wsRuntimePath then
            File.Delete wsRuntimePath
        use outStream = File.OpenWrite wsRuntimePath
        
        let unpackedAssemblies = ResizeArray()
        let metas = ResizeArray()

        let writeTextFile (output, text) =
            Content.Text(text).WriteFile(output)
        let writeBinaryFile (output, bytes) =
            Binary.FromBytes(bytes).WriteFile(output)
        System.IO.Directory.CreateDirectory rootDirectory |> ignore
        let pc = PC.PathUtility.FileSystem(rootDirectory)
        let emit text path =
            match text with
            | Some text ->
                writeTextFile (path, text)
            | None -> ()
        let emitWithMap text path mapping mapFileName mapPath =
            if sourceMap then
                let text =
                    text |> Option.map (fun t ->
                    match mapping with
                    | None -> t
                    | Some _ -> t + ("\n//# sourceMappingURL=" + mapFileName))
                emit text path
                emit mapping mapPath
            else
                emit text path
        let script = PC.ResourceKind.Script
        let content = PC.ResourceKind.Content
        let errors = ResizeArray()
        let settingsUsed = Dictionary()
        let unpackContext =
            if download then 
                let get x =
                    match settingsUsed.TryGetValue(x) with
                    | true, v -> v
                    | false, _ ->
                        let v = getSetting x
                        settingsUsed.[x] <- v
                        v
                Some (
                    {
                        RootFolder = rootDirectory
                        GetSetting = get
                        SourceMap = sourceMap
                    } : Resources.UnpackContext
                )
            else None
        for asm in assemblies do
            let rec printError (e: exn) =
                if isNull e.InnerException then
                    e.Message
                else e.Message + " - " + printError e.InnerException 
                        
            unpackedAssemblies.Add(asm.Name, asm.TimeStamp)
            
            if Option.isNone compMetas then
                match asm.GetResourceStream EMBEDDED_METADATA with
                | Some mstr ->
                    try
                        M.IO.Decode mstr |> metas.Add
                    with _ -> () 
                | _ -> ()
                
            let readResource name =
                asm.GetResourceStream name
                |> Option.map (fun s ->
                    use r = new StreamReader(s)
                    r.ReadToEnd()
                )

            let readResourceBytes name =
                asm.GetResourceStream name
                |> Option.map (fun s ->
                    use m = new MemoryStream()
                    s.CopyTo(m)
                    m.ToArray()
                )

            let aid = PC.AssemblyId.Create(asm.Name)
            
            emitWithMap (readResource EMBEDDED_JS) (pc.JavaScriptPath aid)
                (readResource EMBEDDED_MAP) (pc.MapFileName aid) (pc.MapFilePath aid)
            emitWithMap (readResource EMBEDDED_MINJS) (pc.MinifiedJavaScriptPath aid)
                (readResource EMBEDDED_MINMAP) (pc.MinifiedMapFileName aid) (pc.MinifiedMapFilePath aid)
            let writeText k fn c =
                let p = pc.EmbeddedPath(PC.EmbeddedResource.Create(k, aid, fn))
                writeTextFile (p, c)
            let writeBinary k fn c =
                let p = pc.EmbeddedPath(PC.EmbeddedResource.Create(k, aid, fn))
                writeBinaryFile (p, c)
                
            for resourceName, contentType in asm.WebResources do
                match readResourceBytes resourceName with
                | Some c ->
                    let r = EmbeddedFile.Create(asm.Name, resourceName, c, CT.Parse contentType)
                    if r.IsScript then
                        writeText script r.FileName r.Content
                    else
                        writeBinary content r.FileName (r.GetContentData())
                | _ -> ()
            
            match unpackContext with 
            | Some ctx ->
                try
                    for res in asm.DownloadableResources do
                        try
                            res.Unpack(ctx)
                        with e ->
                            errors.Add <| sprintf "Failed to unpack local resource: %s - %s" (res.GetType().FullName) (printError e)     
                with e ->
                    errors.Add <| sprintf "Failed to unpack local resources: %s" (printError e)   
            | None -> ()
                    
        match compMetas with
        | Some ms -> ms |> Seq.iter metas.Add
        | _ -> ()

        let meta, graph =
            let graph =
                DependencyGraph.Graph.FromData(metas |> Seq.map (fun m -> m.Dependencies))
            let fullMeta =
                { 
                    M.Info.UnionWithoutDependencies metas with
                        Dependencies = graph.GetData()
                }
            let trimmedMeta =
                match filterExpressions with
                | FullMetadata -> fullMeta
                | DiscardExpressions -> fullMeta.DiscardExpressions() 
                | DiscardInlineExpressions -> fullMeta.DiscardInlineExpressions()
                | DiscardNotInlineExpressions -> fullMeta.DiscardNotInlineExpressions()
            
            trimmedMeta, graph
        
        let header =
            {
                DownloadResources = download
                SettingsUsed = settingsUsed |> Seq.map (|KeyValue|) |> Array.ofSeq  
                UseSourceMap = sourceMap
                SourceAssemblies = unpackedAssemblies.ToArray()
                ExpressionOptions = filterExpressions
            }

        UnpackedMetadataEncoding.Encode(outStream, meta, header)
        printfn "Saved precomputed metadata: %s" wsRuntimePath

        outStream.Close()

        meta, graph, List.ofSeq errors