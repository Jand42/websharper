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

module UnpackCommand =
    type Config =
        {
            Assemblies : list<string>
            RootDirectory : string
            UnpackSourceMap : bool
            UnpackTypeScript : bool
            DownloadResources : bool
        }

        static member Create() =
            {
                Assemblies = []
                RootDirectory = "."
                UnpackSourceMap = false
                UnpackTypeScript = false
                DownloadResources = false
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

    let Exec env cmd =
        let baseDir =
            let pathToSelf = typeof<Config>.Assembly.Location
            Path.GetDirectoryName(pathToSelf)
        let wsRuntimePath = Path.Combine(baseDir, "cached.wsruntime") 
        U.Unpack
            cmd.Assemblies wsRuntimePath None
            cmd.RootDirectory cmd.DownloadResources cmd.UnpackSourceMap U.ExpressionOptions.DiscardExpressions 
        |> ignore

        C.Ok

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
