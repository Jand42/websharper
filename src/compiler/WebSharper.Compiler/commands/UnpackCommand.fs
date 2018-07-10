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

namespace WebSharper.Compiler

module PC = WebSharper.PathConventions
module C = Commands
module Re = WebSharper.Core.Resources 
module U = WebSharper.Core.Unpacking 
module S = WebSharper.Core.SettingKeys

module UnpackCommand =
    type Config =
        {
            Assemblies : list<string>
            RootDirectory : string
            UnpackSourceMap : bool
            UnpackTypeScript : bool
            DownloadResources : bool
            Metadatas : seq<WebSharper.Core.Metadata.Info>
        }

        static member Create() =
            {
                Assemblies = []
                RootDirectory = "."
                UnpackSourceMap = false
                UnpackTypeScript = false
                DownloadResources = false
                Metadatas = []
            }

    let GetErrors config =
        [
            for a in config.Assemblies do
                if C.NoFile a then
                    yield "invalid file: " + a
            if C.NoDir config.RootDirectory then
                yield "-root: invalid directory " + config.RootDirectory
        ]

    let Parse args =
        let rec proc opts xs =
            match xs with
            | [] ->
                opts
            | "-root" :: root :: xs ->
                proc { opts with RootDirectory = root } xs
            | "-sm" :: xs ->
                proc { opts with UnpackSourceMap = true } xs
            | "-dts" :: xs ->
                proc { opts with UnpackTypeScript = true } xs
            | x :: xs ->
                proc { opts with Assemblies = x :: opts.Assemblies } xs
        match args with
        | "-unpack" :: root :: args ->
            let def = Config.Create()
            let cfg = { proc def args with RootDirectory = root }
            match GetErrors cfg with
            | [] -> C.Parsed cfg
            | errors -> C.ParseFailed errors
        | "unpack" :: args ->
            let def = Config.Create()
            let cfg = proc def args
            match GetErrors cfg with
            | [] -> C.Parsed cfg
            | errors -> C.ParseFailed errors
        | _ -> C.NotRecognized


    let private localResTyp = typeof<Re.IDownloadableResource>

    type ReferenceAssembly (asm: Assembly) =
        interface U.IAssembly with
            member this.Name = asm.FullName
            member this.GetResourceStream (name: string) = 
                asm.Raw.MainModule.Resources
                |> Seq.tryPick (function
                    | :? Mono.Cecil.EmbeddedResource as r when r.Name = name ->
                        r.GetResourceStream() |> Some
                    | _ -> None
                )
            member this.TimeStamp =
                File.GetLastWriteTimeUtc(asm.LoadPath.Value).ToFileTimeUtc()
            member this.WebResources =
                asm.GetWebResources()
            member this.DownloadableResources =
                let printError (e: exn) =
                    let rec messages (e: exn) =
                        seq {
                            yield e.Message
                            match e with
                            | :? System.Reflection.ReflectionTypeLoadException as e ->
                                yield! Seq.collect messages e.LoaderExceptions
                            | e when isNull e.InnerException -> ()
                            | e -> yield! messages e.InnerException
                        }
                    String.concat " - " (messages e)
                let a = 
                    let path = asm.LoadPath.Value
                    try
                        System.Reflection.Assembly.Load (Path.GetFileNameWithoutExtension path)
                    with e ->
                        if e.HResult <> 0x80131058 then
                            errors.Add <| sprintf "Failed to load assembly for unpacking local resources: %s - %s" p (printError e)     
                        // else this is a reference assembly, so it's ok not to load it.
                        null
                if not (isNull a) then
                    a.GetTypes() |> Seq.choose (fun t ->
                        if t.GetInterfaces() |> Array.contains localResTyp then
                            try
                                Activator.CreateInstance(t) :?> Re.IDownloadableResource |> Some
                            with e ->
                                eprintfn "Failed to unpack local resource: %s - %s" t.FullName (printError e) 
                                None
                        else None
                    )
                else Seq.empty

    let Exec env cmd =
        let baseDir =
            let pathToSelf = typeof<Config>.Assembly.Location
            Path.GetDirectoryName(pathToSelf)
        let aR =
            AssemblyResolver.Create()
                .WithBaseDirectory(baseDir)
                .SearchDirectories([baseDir])
        let pc = PC.PathUtility.FileSystem(cmd.RootDirectory)
        let aR = aR.SearchPaths(cmd.Assemblies)
        let loader = Loader.Create aR stderr.WriteLine

        let assemblies =
            cmd.Assemblies |> Seq.choose (fun p ->
                try Some (ReferenceAssembly (loader.LoadFile p) :> U.IAssembly)
                with _ -> None
            )
        
        let baseDir =
            let pathToSelf = typeof<Config>.Assembly.Location
            Path.GetDirectoryName(pathToSelf)
        let wsRuntimePath = Path.Combine(baseDir, "cached.wsruntime") 
        let getSettings x =
            match x with
            | S.UseDownloadedResources -> Some (string cmd.DownloadResources)
            | S.WebSharperSourceMaps -> Some (string cmd.UnpackSourceMap)
            | S.WebSharperSharedMetadata -> Some "DiscardExpressions"
            | _ -> None

        let _, _, errors =
            U.Unpack assemblies wsRuntimePath (Some cmd.Metadatas) cmd.RootDirectory getSettings

        match errors with
        | [] -> C.Ok
        | _ -> C.Errors errors
        
    let Description =
        "unpacks resources from WebSharper assemblies"

    let Usage =
        [
            "Usage: WebSharper.exe unpack [OPTIONS] assembly.dll ..."
            "-root <dir>    Path to web project root directory"
            "-sm            Unpack source maps and source files"
            //"-dts           Unpack TypeScript declaration files"
        ]
        |> String.concat System.Environment.NewLine

    let Instance =
        C.DefineCommand<Config> "unpack" Description Usage Parse Exec
