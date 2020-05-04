// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type private RuntimeData =
    | NotInitialized
    | Initialized of M.Info * Graph * J.Provider 

let mutable private data = NotInitialized 

let GetMetadata() =
    match data with
    | NotInitialized ->
        failwith "Shared metadata is accessed before it is initialized"
    | Initialized (meta, _, _) -> meta 
   
let GetDependencies() =
    match data with
    | NotInitialized ->
        failwith "Shared dependency graph is accessed before it is initialized"
    | Initialized (_, deps, _) -> deps 
   
let GetJson() =
    match data with
    | NotInitialized ->
        failwith "Shared json provider is accessed before it is initialized"
    | Initialized (_, _, json) -> json 
   
let PlainJson = WebSharper.Json.ServerSideProvider

let private lockObject = obj()

let Initialize (binDir, wwwRoot) =
    if data = NotInitialized then 
        lock lockObject <| fun () ->
        if data = NotInitialized then 
            let trace =
                System.Diagnostics.TraceSource("WebSharper",
                    System.Diagnostics.SourceLevels.All)
            let before = System.DateTime.UtcNow
            let assemblies =
                WebSharper.Core.Resources.AllReferencedAssemblies.Value
                |> List.map (fun a -> U.RuntimeAssembly a :> U.IAssembly)

            let wsRuntimePath = Path.Combine(binDir, "cached.wsruntime") 

            let downloadResources =
                match Context.GetSetting "WebSharperDownloadResources" with
                | Some s -> 
                    match bool.TryParse(s) with
                    | true, dl -> dl
                    | _ -> false
                | None -> false

            let sourceMap =
                match Context.GetSetting "WebSharperSourceMap" with
                | Some s -> 
                    match bool.TryParse(s) with
                    | true, sm -> sm
                    | _ -> false
                | None -> false

            let expressionOptions =
                match Context.GetSetting "WebSharperSharedMetadata" with
                | Some "None" -> U.ExpressionOptions.NoMetadata
                | Some "Inlines" -> U.ExpressionOptions.DiscardNotInlineExpressions
                | Some "NotInlines" -> U.ExpressionOptions.DiscardInlineExpressions
                | Some "Full" | None -> U.ExpressionOptions.FullMetadata
                | _ -> U.ExpressionOptions.DiscardExpressions

            let m, d =
                U.Unpack
                    assemblies wsRuntimePath None
                    wwwRoot downloadResources sourceMap expressionOptions

            let after = System.DateTime.UtcNow
            trace.TraceInformation("Initialized WebSharper in {0} seconds.",
                (after-before).TotalSeconds)
        
            data <- Initialized (m, d, J.Provider.CreateTyped m)

[<Literal>]
let SCRIPT_MANAGER_ID = "WebSharperScriptManager"
