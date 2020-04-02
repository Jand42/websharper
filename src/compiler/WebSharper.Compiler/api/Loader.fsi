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

namespace WebSharper.Compiler

/// Loads assemblies.
[<Sealed>]
type Loader =

    /// Creates a new loader. Accepts an assembly resolver.
    static member Create : resolver: AssemblyResolver -> log: (string -> unit) -> Loader

    /// Loads an assembly from raw data.
    member LoadRaw : byte [] -> option<Symbols> -> Assembly

    /// Loads an assembly from a given path.
    member LoadFile : path: string * ?loadSymbols: bool -> Assembly

module internal LoaderUtility =

    [<Sealed>]
    type Resolver =
        interface Mono.Cecil.IAssemblyResolver
        new : AssemblyResolver -> Resolver
        member Resolve : string -> Mono.Cecil.AssemblyDefinition
        member Resolve : string * Mono.Cecil.ReaderParameters -> Mono.Cecil.AssemblyDefinition
        member Resolve : Mono.Cecil.AssemblyNameReference -> Mono.Cecil.AssemblyDefinition
        member Resolve : Mono.Cecil.AssemblyNameReference * Mono.Cecil.ReaderParameters -> Mono.Cecil.AssemblyDefinition
