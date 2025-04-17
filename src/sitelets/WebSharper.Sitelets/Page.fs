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

namespace WebSharper.Sitelets

open System.Collections.Generic
open System.Threading.Tasks
open WebSharper
open WebSharper.Web
type private HtmlTextWriter = WebSharper.Core.Resources.HtmlTextWriter
type private Writer = HtmlTextWriter -> Task

type Page =
    {
        Doctype : option<string>
        Title : option<string>
        Renderer : option<string> -> option<string> -> Writer -> Writer ->
            HtmlTextWriter -> Task
        Head : seq<INode>
        Body : seq<INode>
    }

    static member Default =
        let renderer (doctype : option<string>) (title: option<string>)
            (writeHead : Writer) (writeBody : Writer) (writer: HtmlTextWriter) =
            task {
                // Doctype
                match doctype with
                | Some dt -> do! writer.WriteLineAsync dt
                | None -> ()
                do! writer.RenderBeginTag "html"
                // Head section
                do! writer.RenderBeginTag "head"
                match title with
                | Some t ->
                    do! writer.WriteFullBeginTag "title"
                    do! writer.WriteAsync t
                    do! writer.WriteEndTag "title"
                    do! writer.WriteLineAsync()
                | None -> ()
                do! writeHead writer
                do! writer.RenderEndTag()
                // Body section
                do! writer.RenderBeginTag "body"
                do! writeBody writer
                do! writer.RenderEndTag()
                do! writer.RenderEndTag()
            } :> Task
        {
            Doctype = Some "<!DOCTYPE html>"
            Title = None
            Head = []
            Renderer = renderer
            Body = []
        }
