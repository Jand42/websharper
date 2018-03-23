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

namespace WebSharper.Core

module CT = ContentTypes

type EmbeddedFile =
    {
        ResAssembly : string
        mutable ResContent : string
        ResContentBytes : byte []
        ResContentType : CT.ContentType
        ResName : string
    }

    static member Create(assemblyFullName, resourceName, bytes, contentType) =
        {
            ResAssembly = assemblyFullName
            ResContent = null
            ResContentBytes = bytes
            ResContentType = contentType
            ResName = resourceName
        }

    member ri.GetContentData() =
        Array.copy ri.ResContentBytes

    member ri.Content =
        match ri.ResContent with
        | null ->
            try
                let s = System.Text.UTF8Encoding(false, true).GetString(ri.ResContentBytes)
                ri.ResContent <- s
                s
            with e ->
                failwithf "Encoding problem in resource [%s] in assembly [%s]: %O"
                    ri.ResAssembly ri.ResName e
        | s -> s

    member ri.ContentType = ri.ResContentType
    member ri.FileName = ri.ResName

    member ri.IsScript =
        match ri.ResContentType with
        | CT.JavaScript -> true
        | _ -> false
