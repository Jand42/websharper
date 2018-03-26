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

namespace WebSharper.Web

open System.Collections.Generic
module J = WebSharper.Core.Json
module M = WebSharper.Core.Metadata
module U = WebSharper.Core.Unpacking 
open WebSharper.Core.DependencyGraph
open System.Reflection
open System.IO

[<AbstractClass>]
type Shared() =
    static let lockObject = obj()
    static let mutable meta = Unchecked.defaultof<M.Info>
    static let mutable deps = Unchecked.defaultof<Graph>
    static let mutable json = Unchecked.defaultof<J.Provider>

    static member Metadata = 
        if obj.ReferenceEquals(meta, null) then
            failwith "Shared metadata is accessed before it is initialized"
        meta
    static member Dependencies = 
        if obj.ReferenceEquals(meta, null) then
            failwith "Shared dependency graph is accessed before it is initialized"
        deps
    static member Json = 
        if obj.ReferenceEquals(meta, null) then
            failwith "Shared json provider is accessed before it is initialized"
        json
    static member PlainJson = WebSharper.Json.ServerSideProvider

    static member Initialize (binDir, wwwRoot) =
        if obj.ReferenceEquals(meta, null) then
            lock lockObject <| fun () ->
            let trace =
                System.Diagnostics.TraceSource("WebSharper",
                    System.Diagnostics.SourceLevels.All)
            let before = System.DateTime.UtcNow
            let assemblies =
                WebSharper.Core.Resources.AllReferencedAssemblies.Value
                |> List.map (fun a -> a.Location)

            let wsRuntimePath = Path.Combine(binDir, "cached.wsruntime") 

            let m, d =
                U.Unpack
                    assemblies wsRuntimePath None
                    wwwRoot true true U.ExpressionOptions.DiscardExpressions 

            let after = System.DateTime.UtcNow
            trace.TraceInformation("Initialized WebSharper in {0} seconds.",
                (after-before).TotalSeconds)
        
            meta <- m
            deps <- d
            json <- J.Provider.CreateTyped m

module Shared =

    [<Literal>]
    let SCRIPT_MANAGER_ID = "WebSharperScriptManager"
