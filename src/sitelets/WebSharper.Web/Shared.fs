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

module WebSharper.Web.Shared

open System.Collections.Generic
module J = WebSharper.Core.Json
module M = WebSharper.Core.Metadata
module U = WebSharper.Core.Unpacking 
open WebSharper.Core.DependencyGraph
open System.Reflection
open System.IO

let private trace =
    System.Diagnostics.TraceSource("WebSharper",
        System.Diagnostics.SourceLevels.All)

let private loadMetadata () =
    let before = System.DateTime.UtcNow
    //let filterExpressions : M.Info -> M.Info =
    //    match Context.GetSetting "WebSharperSharedMetadata" with
    //    | Some "Inlines" -> fun m -> m.DiscardNotInlineExpressions()
    //    | Some "NotInlines" -> fun m -> m.DiscardInlineExpressions()
    //    | Some "Full" | None -> id
    //    | _ -> fun m -> m.DiscardExpressions()
    //let metas =
    //    WebSharper.Core.Resources.AllReferencedAssemblies.Value
    //    |> Seq.choose M.IO.LoadReflected
    //    |> Seq.map filterExpressions
    //    |> Seq.toList

    let assemblies =
        WebSharper.Core.Resources.AllReferencedAssemblies.Value
        |> List.map (fun a -> a.Location)

    let baseDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let wsRuntimePath = Path.Combine(baseDir, "cached.wsruntime") 

    let rootDirectory = Path.Combine(baseDir, "..") // TODO get correct root dir

    let res =
        U.Unpack
            assemblies wsRuntimePath None
            rootDirectory true true U.ExpressionOptions.DiscardExpressions 

    let after = System.DateTime.UtcNow
    trace.TraceInformation("Initialized WebSharper in {0} seconds.",
        (after-before).TotalSeconds)
    res

let Metadata, Dependencies = loadMetadata () 

let Json = J.Provider.CreateTyped Metadata

let PlainJson = WebSharper.Json.ServerSideProvider

[<Literal>]
let internal SCRIPT_MANAGER_ID = "WebSharperScriptManager"
