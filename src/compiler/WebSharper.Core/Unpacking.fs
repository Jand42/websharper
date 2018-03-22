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

type ExpressionOptions =
    | FullMetadata
    | DiscardExpressions
    | DiscardInlineExpressions
    | DiscardNotInlineExpressions

type RuntimeHeader =
    {
        DownloadResources : bool
        UseSourceMap : bool
        UnpackedFiles : string[]
        ExpressionOptions : ExpressionOptions
    }

let UnpackedMetadataEncoding =
    try
        M.IO.EncodingProvider.DeriveEncodingWithHeader(typeof<RuntimeHeader>, typeof<M.Info>, M.IO.CurrentVersion)
    with B.NoEncodingException t ->
        failwithf "Failed to create binary encoder for type %s" t.FullName

let Unpack (assemblies: list<string>) (wsRuntimePath: string) (preMerged: option<M.Info>) (rootDirectory: string) (download: bool) (sourceMap: bool) (options: ExpressionOptions) =
    
    let failIfHeaderNotUpToDate (h: obj) =
        match h with
        | :? RuntimeHeader as header ->
            if download && not header.DownloadResources then failwith "Downloading external resources was turned on since latest unpack"        
            if sourceMap && not header.UseSourceMap then failwith "Source mapping was turned on since latest unpack"        
            if not sourceMap && header.UseSourceMap then failwith "Source mapping was turned off since latest unpack"     
            if options <> header.ExpressionOptions then failwith "Expression kinds to keep in runtime metadata was changed since last unpack"     
            let wsRuntimeTimestamp = File.GetLastWriteTimeUtc wsRuntimePath
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
    | Some m -> m
    | _ ->

        use outStream = File.OpenWrite wsRuntimePath
        
        let unpackedFiles = ResizeArray()

        let writeTextFile (output, text) =
            Content.Text(text).WriteFile(output)
        let writeBinaryFile (output, bytes) =
            Binary.FromBytes(bytes).WriteFile(output)
        System.IO.Directory.CreateDirectory rootDirectory |> ignore
        let rc = PC.PathUtility.FileSystem("")
        let pc = PC.PathUtility.FileSystem(rootDirectory)
        let emit text path =
            match text with
            | Some text -> writeTextFile (path, text)
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

            let aid = PC.AssemblyId.Create(Path.GetFileNameWithoutExtension p)
            
            emitWithMap a.ReadableJavaScript (pc.JavaScriptPath aid)
                a.MapFileForReadable (pc.MapFileName aid) (pc.MapFilePath aid)
            emitWithMap a.CompressedJavaScript (pc.MinifiedJavaScriptPath aid)
                a.MapFileForCompressed (pc.MinifiedMapFileName aid) (pc.MinifiedMapFilePath aid)
            if cmd.UnpackTypeScript then
                emit a.TypeScriptDeclarations (pc.TypeScriptDefinitionsPath aid)
            let writeText k fn c =
                let p = pc.EmbeddedPath(PC.EmbeddedResource.Create(k, aid, fn))
                writeTextFile (p, c)
            let writeBinary k fn c =
                let p = pc.EmbeddedPath(PC.EmbeddedResource.Create(k, aid, fn))
                writeBinaryFile (p, c)
            for r in a.GetScripts() do
                writeText script r.FileName r.Content
            for r in a.GetContents() do
                writeBinary content r.FileName (r.GetContentData())
            
            if cmd.DownloadResources then
                try
                    let asm = 
                        try
                            System.Reflection.Assembly.Load (Path.GetFileNameWithoutExtension p)
                        with e ->
                            eprintfn "Failed to load assembly for unpacking local resources: %s - %s" p (printError e)     
                            null
                    if not (isNull asm) then
                        for t in asm.GetTypes() do
                            if t.GetInterfaces() |> Array.contains localResTyp then
                                try
                                    let res = Activator.CreateInstance(t) :?> Re.IDownloadableResource
                                    res.Unpack(cmd.RootDirectory)
                                with e ->
                                    eprintfn "Failed to unpack local resource: %s - %s" t.FullName (printError e)     
                with e ->
                    eprintfn "Failed to unpack local resources: %s" (printError e)   
                    
        let meta = failwith "TODO"

        meta
