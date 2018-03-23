// $begin{copyright}
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

open System.IO
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
        UseSourceMap : bool
        SourceAssemblies : string[]
        UnpackedFiles : string[]
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

let Unpack
    (assemblies: list<string>) (wsRuntimePath: string) (preMerged: option<M.Info>)
    (rootDirectory: string) (download: bool) (sourceMap: bool) (options: ExpressionOptions) =
    
    let failIfHeaderNotUpToDate (h: obj) =
        match h with
        | :? RuntimeHeader as header ->
            if download && not header.DownloadResources then failwith "Downloading external resources was turned on since latest unpack"        
            if sourceMap && not header.UseSourceMap then failwith "Source mapping was turned on since latest unpack"        
            if not sourceMap && header.UseSourceMap then failwith "Source mapping was turned off since latest unpack"     
            if options <> header.ExpressionOptions then failwith "Expression kinds to keep in runtime metadata was changed since last unpack"     
            let wsRuntimeTimestamp = File.GetLastWriteTimeUtc wsRuntimePath
            // TODO for a in header.SourceAssemblies do
            for f in header.UnpackedFiles do
                let fp = Path.Combine(rootDirectory, f)
                if not <| File.Exists fp then
                    failwithf "Unpacked file was not found: %s" f
                if File.GetLastWriteTimeUtc fp < wsRuntimeTimestamp then
                    failwithf "Unpacked file has older timestamp than wsruntime: %s" f

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
        meta, DependencyGraph.Graph.FromData(meta.Dependencies)
    | _ ->

        if File.Exists wsRuntimePath then
            File.Delete wsRuntimePath
        use outStream = File.OpenWrite wsRuntimePath
        
        let unpackedFiles = ResizeArray()
        let unpackedAssemblies = ResizeArray()
        let metas = ResizeArray()

        let writeTextFile (output, text) =
            Content.Text(text).WriteFile(output)
        let writeBinaryFile (output, bytes) =
            Binary.FromBytes(bytes).WriteFile(output)
        System.IO.Directory.CreateDirectory rootDirectory |> ignore
        let rc = PC.PathUtility.FileSystem("")
        let pc = PC.PathUtility.FileSystem(rootDirectory)
        let emit text path shortPath =
            match text with
            | Some text ->
                writeTextFile (path, text)
                unpackedFiles.Add shortPath
            | None -> ()
        let emitWithMap text path shortPath mapping mapFileName mapPath mapShortPath =
            if sourceMap then
                let text =
                    text |> Option.map (fun t ->
                    match mapping with
                    | None -> t
                    | Some _ -> t + ("\n//# sourceMappingURL=" + mapFileName))
                emit text path shortPath
                emit mapping mapPath mapShortPath
            else
                emit text path shortPath
        let script = PC.ResourceKind.Script
        let content = PC.ResourceKind.Content
        let localResTyp = typeof<Re.IDownloadableResource>
        for p in assemblies do
            let rec printError (e: exn) =
                if isNull e.InnerException then
                    e.Message
                else e.Message + " - " + printError e.InnerException 
            let asm = 
                try
                    System.Reflection.Assembly.LoadFrom p
                with e ->
                    eprintfn "Failed to load assembly for unpacking WebSharper resources: %s - %s" p (printError e)     
                    null
            
            if asm.GetManifestResourceNames() |> Array.contains EMBEDDED_METADATA then
            
                unpackedAssemblies.Add(Path.GetFileName p)
                metas.Add(M.IO.LoadReflected(asm))
                let a = asm.FullName
                
                let readResource name =
                    use s = asm.GetManifestResourceStream name
                    if isNull s then None else
                    use r = new StreamReader(s)
                    Some (r.ReadToEnd())

                let readResourceBytes name =
                    use s = asm.GetManifestResourceStream name
                    if isNull s then None else
                    use m = new MemoryStream()
                    s.CopyTo(m)
                    Some (m.ToArray())

                let aid = PC.AssemblyId.Create(a)
            
                emitWithMap (readResource EMBEDDED_JS) (pc.JavaScriptPath aid) (rc.JavaScriptPath aid)
                    (readResource EMBEDDED_MAP) (pc.MapFileName aid) (pc.MapFilePath aid) (rc.MapFilePath aid)
                emitWithMap (readResource EMBEDDED_MINJS) (pc.MinifiedJavaScriptPath aid) (rc.MinifiedJavaScriptPath aid)
                    (readResource EMBEDDED_MINMAP) (pc.MinifiedMapFileName aid) (pc.MinifiedMapFilePath aid) (rc.MinifiedMapFilePath aid)
                let writeText k fn c =
                    let p = pc.EmbeddedPath(PC.EmbeddedResource.Create(k, aid, fn))
                    writeTextFile (p, c)
                let writeBinary k fn c =
                    let p = pc.EmbeddedPath(PC.EmbeddedResource.Create(k, aid, fn))
                    writeBinaryFile (p, c)
                
                let webResources =
                    asm.CustomAttributes
                    |> Seq.choose (fun attr ->
                        if IsWebResourceAttribute attr.AttributeType.FullName then
                            match Seq.toList attr.ConstructorArguments with
                            | [StringArg resourceName; StringArg contentType] ->
                                readResourceBytes resourceName
                                |> Option.map (fun c ->
                                    EmbeddedFile.Create(string a, resourceName, c, CT.Parse contentType))
                            | _ -> None
                        else None)

                for r in webResources do
                    if r.IsScript then
                        writeText script r.FileName r.Content
                    else
                        writeBinary content r.FileName (r.GetContentData())
            
                if download then
                    try
                        for t in asm.GetTypes() do
                            if t.GetInterfaces() |> Array.contains localResTyp then
                                try
                                    let res = System.Activator.CreateInstance(t) :?> Re.IDownloadableResource
                                    res.Unpack(rootDirectory)
                                with e ->
                                    eprintfn "Failed to unpack remote resource: %s - %s" t.FullName (printError e)     
                    with e ->
                        eprintfn "Failed to unpack remote resources: %s" (printError e)   
                    
        let graph =
            DependencyGraph.Graph.NewWithDependencyAssemblies(metas |> Seq.choose id |> Seq.map (fun m -> m.Dependencies))
        let fullMeta =
            M.Info.UnionWithoutDependencies (metas |> Seq.choose id)
            //{ 
            //    M.Info.UnionWithoutDependencies (metas |> Seq.choose id) with
            //        Dependencies = graph.GetData()
            //}

        let trimmedMeta =
            match options with
            | FullMetadata -> fullMeta
            | DiscardExpressions -> fullMeta.DiscardExpressions() 
            | DiscardInlineExpressions -> fullMeta.DiscardInlineExpressions()
            | DiscardNotInlineExpressions -> fullMeta.DiscardNotInlineExpressions()
        
        let header =
            {
                DownloadResources = download
                UseSourceMap = sourceMap
                SourceAssemblies = unpackedAssemblies.ToArray()
                UnpackedFiles = unpackedFiles.ToArray()
                ExpressionOptions = options
            }

        UnpackedMetadataEncoding.Encode(outStream, trimmedMeta, header)
        printfn "Saved precomputed metadata: %s" wsRuntimePath

        trimmedMeta, graph
