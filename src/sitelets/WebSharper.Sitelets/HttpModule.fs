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

namespace WebSharper.Sitelets

#if NET461 // ASP.NET: Sitelets HttpModule

open System
open System.Collections.Generic
open System.Collections.Specialized
open System.Configuration
open System.Diagnostics
open System.IO
open System.Reflection
open System.Web
open System.Web.Compilation
open System.Web.Hosting
open WebSharper.Web
module R = WebSharper.Core.Remoting

module private WebUtils =

    let [<Literal>] HttpContextKey = "HttpContext"

    let getUri (req: HttpRequestBase) : Uri =
        match req.ApplicationPath with
        | "" | "/" -> req.Url
        | _ ->
            if req.Url.IsAbsoluteUri then
                let uB = UriBuilder req.Url
                if uB.Path.StartsWith(req.ApplicationPath) then
                    uB.Path <- uB.Path.Substring(req.ApplicationPath.Length)
                uB.Uri
            else
                req.Url

    type AspNetRequest(ctx: HttpContextBase) =
        inherit Http.Request()

        let req = ctx.Request
        let method = Http.Method.OfString req.HttpMethod
        let uri = getUri req
        let headers =
            seq {
                for key in req.Headers.AllKeys do
                    yield Http.Header.Custom key req.Headers.[key]
            }
        let mutable post = null
        let mutable get = null
        let mutable serverVars = null
        let mutable cookies = null

        override this.Method = method
        override this.Uri = uri                                     
        override this.Headers = headers
        override this.Body = req.InputStream
        override this.Post = 
            if isNull post then
                post <- Http.ParametersFromNameValues(req.Form)
            post
        override this.Get = 
            if isNull get then
                get <- Http.ParametersFromNameValues(req.QueryString)
            get
        override this.ServerVariables = 
            if isNull serverVars then
                serverVars <- Http.ParametersFromNameValues(req.ServerVariables)
            serverVars
        override this.Files =
            let fs = req.Files
            seq {
                for i = 0 to fs.Count - 1 do
                    let f = fs.[i]
                    let k = fs.GetKey(i)
                    yield { new Http.IPostedFile with
                        member this.Key = k
                        member this.ContentLength = f.ContentLength
                        member this.ContentType = f.ContentType
                        member this.FileName = f.FileName
                        member this.InputStream = f.InputStream
                        member this.SaveAs(n) = f.SaveAs(n)
                    }
            }
        override this.Cookies =
            if isNull cookies then
                let d = NameValueCollection()
                match headers |> Seq.tryFind (fun h -> h.Name = "Cookie") with
                | None -> ()
                | Some h ->
                    for s in h.Value.Split([|"; "|], StringSplitOptions.None) do
                        match s.IndexOf '=' with
                        | -1 -> failwith "Cookie header syntax invalid"
                        | i -> d.Add(s.[..i-1], s.[i+1..])
                cookies <- Http.ParametersFromNameValues d
            cookies

    /// Converts ASP.NET requests to Sitelet requests.
    let convertRequest (ctx: HttpContextBase) : Http.Request =
        AspNetRequest ctx :> _

    /// Constructs the sitelet context object.
    let getContext (site: Sitelet<obj>) (ctx: HttpContextBase) resCtx appPath rootFolder (request: Http.Request) =
        new Context<obj>(
            ApplicationPath = appPath,
            Json = Shared.GetJson(),
            Link = (fun action ->
                match site.Router.Link action with
                | Some loc ->
                    if loc.IsAbsoluteUri then string loc else
                        joinWithSlash appPath (string loc)
                | None -> failwith "Failed to link to action"),            
            Metadata = Shared.GetMetadata(),
            Dependencies = Shared.GetDependencies(),
            ResourceContext = resCtx,
            Request = request,
            RootFolder = rootFolder,
            UserSession = new AspNetFormsUserSession(ctx),
            Environment = Map [HttpContextKey, box ctx]
        )

    /// Writes a response.
    let respond (site: Sitelet<obj>) (ctx: HttpContextBase) resCtx appPath rootFolder (req: Http.Request) (action: obj) =
        // Create a context
        let context = getContext site ctx resCtx appPath rootFolder req
        // Handle action
        // we use AsyncBuilder directly so there is no .Delay, .For, .Combine calls for minimal overhead
        async.Bind(
            Content.ToResponse (site.Controller.Handle action) context,
            fun response ->
                let resp = ctx.Response
                resp.Status <- response.Status.ToString()
                for header in response.Headers do
                    resp.AddHeader(header.Name, header.Value)
                if req.Cookies.[RpcHandler.CsrfTokenKey].IsNone then
                    RpcHandler.SetCsrfCookie resp
                response.WriteBody resp.OutputStream
                resp.End()
                async.Zero()
        )

/// The ISS handler for WebSharper applications.
[<Sealed>]
type HttpHandler(request: Http.Request, action: obj, site: Sitelet<obj>, resCtx, appPath, rootFolder) =
    let processRequest ctx = WebUtils.respond site ctx resCtx appPath rootFolder request action
    let (beginAction, endAction, cancelAction) = Async.AsBeginEnd (fun ctx -> processRequest ctx)

    interface SessionState.IRequiresSessionState

    interface IHttpHandler with
        member this.IsReusable = false
        member this.ProcessRequest(ctx) = this.ProcessRequest(HttpContextWrapper(ctx)) |> Async.RunSynchronously

    interface IHttpAsyncHandler with
        member this.BeginProcessRequest(ctx, cb, _) = beginAction (HttpContextWrapper(ctx), cb, null)
        member this.EndProcessRequest(result) = endAction result

    member this.ProcessRequest(ctx) = processRequest ctx

/// IIS module, processing the URLs and serving the pages.
[<Sealed>]
type HttpModule() =

    let mutable runtime = None

    let tryGetHandler (ctx: HttpContextBase) =
        runtime
        |> Option.bind (fun (site, resCtx, appPath, rootFolder) ->
            let request = WebUtils.convertRequest ctx
            site.Router.Route(request)
            |> Option.map (fun action ->
                HttpHandler(request, action, site, resCtx, appPath, rootFolder)))

    let remap (ctx: HttpContextBase) (h: HttpHandler) =
        if HttpRuntime.UsingIntegratedPipeline then
            ctx.RemapHandler(h)
        else
            ctx.Handler <- h

    do  Context.IsDebug <- fun () -> HttpContext.Current.IsDebuggingEnabled
        Context.GetSetting <- fun s ->
            ConfigurationManager.AppSettings.[s]
            |> Option.ofObj

    /// If true, Sitelets take priority over certain other handlers such as ASP.NET MVC if their URL space overlaps.
    /// Default: true.
    static member val OverrideHandler = true with get, set

    interface IHttpModule with
        member this.Init app =
            let appPath = HttpRuntime.AppDomainAppVirtualPath
            let rootFolder = HttpRuntime.AppDomainAppPath
            let getSetting x =
                match app.Context.Items.[x] with
                | null -> None
                | s -> Some (string s)
            Shared.Initialize (Path.Combine(rootFolder, "bin")) rootFolder getSetting
            runtime <- Some (
                Loading.SiteletDefinition,
                ResourceContext.ResourceContext appPath,
                appPath,
                rootFolder
            )
            let handler =
                new EventHandler(fun x _ ->
                    let ctx = HttpContextWrapper((x :?> HttpApplication).Context)
                    if not (RpcHandler.IsRemotingRequest(ctx.Request)) then
                        tryGetHandler ctx |> Option.iter (fun h ->
                            ctx.Items.["WebSharper.SiteletHandler"] <- h
                            remap ctx h))
            if HttpRuntime.UsingIntegratedPipeline then
                app.add_PostAuthorizeRequest(handler)
                // This is needed to override ASP.NET MVC:
                if HttpModule.OverrideHandler then
                    app.add_PostResolveRequestCache(new EventHandler(fun x _ ->
                        let ctx = (x :?> HttpApplication).Context
                        match ctx.Items.["WebSharper.SiteletHandler"] with
                        | null -> ()
                        | h -> remap (HttpContextWrapper ctx) (unbox h)
                    ))
            else 
                app.add_PostMapRequestHandler(handler)

        member this.Dispose() = ()

    member this.TryProcessRequest(ctx: HttpContextBase) : option<Async<unit>> =
        tryGetHandler ctx
        |> Option.map (fun h -> h.ProcessRequest(ctx))

#endif
