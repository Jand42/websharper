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

/// Manages resources, such as dependent JavaScript and CSS files.
module WebSharper.Core.Resources

open System
open System.Threading.Tasks

type HtmlTextWriter =
    interface IDisposable
    member WriteAsync : string -> Task
    member WriteLineAsync : unit -> Task
    member WriteLineAsync : string -> Task
    member RenderBeginTag : string -> Task
    member RenderEndTag : unit -> Task
    member WriteBeginTag : string -> Task
    member WriteFullBeginTag : string -> Task
    member WriteEndTag : string -> Task
    member WriteEncodedText : string -> Task
    member AddAttribute : string * string -> unit
    member WriteAttribute : string * string -> Task
    member WriteAttribute : string * string * encode: bool -> Task
    static member SelfClosingTagEnd : string
    static member TagLeftChar : char
    static member TagRightChar : char
    new : System.IO.TextWriter -> HtmlTextWriter
    new : System.IO.TextWriter * indent: string -> HtmlTextWriter
    static member IsSelfClosingTag : string -> bool
    member WriteStartCode : scriptBaseUrl: option<string> * ?includeScriptTag: bool * ?skipAssemblyDir: bool * ?activation: (string -> unit) * ?bundleNames: string[] -> Task
    static member WriteStartCode : writer: System.IO.TextWriter * scriptBaseUrl: option<string> * ?includeScriptTag: bool * ?skipAssemblyDir: bool * ?activation: (string -> unit) * ?bundleNames: string[] -> Task

type MediaType =
    | Css
    | Js
    | JsModule

type RenderLocation =
    | Scripts
    | Styles
    | Meta

/// Defines how to render a resource.
type Rendering =
    | RenderInline of string
    | RenderLink of string
    | Skip

    member Emit : HtmlTextWriter * MediaType * ?defaultToHttp: bool -> Task
    member Emit : (RenderLocation -> HtmlTextWriter) * MediaType * ?defaultToHttp: bool -> Task
    static member GetWebResourceRendering : ctx: Context * resource: System.Type * filename: string -> Rendering
    static member RenderCached : ctx: Context * resource: IResource * getWriter : (RenderLocation -> HtmlTextWriter) -> Task

/// Defines the context in which resources can be rendered.
and Context =
    {
        /// Transform foreign links to starting with `//` to `http://`.
        DefaultToHttp : bool

        ///// Gets local resource hash values.
        //GetResourceHash : string * string -> int

        /// Get the base URL path for WebSharper scripts.
        ScriptBaseUrl : option<string>

        /// Provides a configuration settings collection.
        GetSetting : string -> option<string>

        /// Constructs URLs to point to embedded resources.
        GetWebResourceRendering : Type -> string -> Rendering

        /// Gets the web application root URL. 
        WebRoot : string  
        
        /// Cache for resolved rendering of resources.
        RenderingCache : System.Collections.Concurrent.ConcurrentDictionary<IResource, (RenderLocation -> HtmlTextWriter) -> Task>

        /// Cache for resolved dependency lookups.
        ResourceDependencyCache : System.Collections.Concurrent.ConcurrentDictionary<Metadata.Node Set, IResource list>
    }

/// An interface for resource-defining types.
and IResource =

    /// Renders the resource to a given TextWriter.
    abstract member Render : Context -> ((RenderLocation -> HtmlTextWriter) -> Task)

/// A resource value appending nothing.
val EmptyResource : IResource

/// An interface for resources to execute custom unpack.
type IDownloadableResource =

    /// Gets the WebSharper output root directory.
    abstract Unpack : string -> unit    

    /// Returns JavaScript imports to include in .ts files depending on this resource.
    abstract member GetImports : unit -> string[]

/// An interface for resources whose JavaScript code, if any,
/// is a sequence of external scripts with the given URLs.
type IExternalScriptResource =
    inherit IResource

    /// Get the JavaScript script URL(s) for this resource.
    /// Can return an empty array if this is not a JavaScript resource.
    abstract member Urls : Context -> string[]

/// A helper base class for resource-defining types.
type BaseResource =

    /// References an embedded resource from he current assembly.
    /// The string represents either a reference to an embedded
    /// resource from the current assembly, or a (possibly relative)
    /// URL. CSS resources are distinguished from JavaScript resources by
    /// checking for the .css suffix.
    new : string -> BaseResource

    /// References several external resources by specifying the
    /// absolute base URL and one or more relative URLs.
    /// The base URL is made configurable by consulting the application
    /// setting matching the full name of the declaring type.
    /// CSS resources are distinguished from JavaScript resources
    /// by checking for the .css syntax.
    new : string * string * [<ParamArray>] xs: string [] -> BaseResource

    interface IResource
    interface IDownloadableResource
    interface IExternalScriptResource

/// Represents the runtime library resource required by all WebSharper code.
[<Sealed>]
type Runtime =
    new : unit -> Runtime
    interface IResource
    static member Instance : IResource
