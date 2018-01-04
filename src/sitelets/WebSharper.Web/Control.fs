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

open WebSharper
open WebSharper.Core

module M = WebSharper.Core.Metadata
module R = WebSharper.Core.AST.Reflection
//module P = WebSharper.Core.JavaScript.Packager

/// A server-side control that adds a runtime dependency on a given resource.
type Require (t: System.Type, [<System.ParamArray>] parameters: obj[]) =
    inherit System.Web.UI.Control()

    let t = AST.Reflection.ReadTypeDefinition t
    let req = 
        [M.ResourceNode (t, 
            if parameters.Length = 0 then None else Some(M.ParameterObject.OfObj parameters))]

    interface INode with
        member this.Write(_, _) = ()
        member this.IsAttribute = false

    interface IRequiresResources with
        member this.Encode(_, _) = []
        member this.Requires(_) = req :> _

    override this.OnLoad _ =
        this.ID <-
            ScriptManager.Find(base.Page)
                .Register(None, this, Shared.Metadata, Shared.Json)

    override this.Render _ = ()

/// A server-side control that adds a runtime dependency on a given resource.
type Require<'T when 'T :> Resources.IResource>() =
    inherit Require(typeof<'T>)

/// A base class for defining custom ASP.NET controls. Inherit from this class,
/// override the Body property and use the new class as a Server ASP.NET
/// control in your application.
[<AbstractClass>]
type Control() =
    inherit System.Web.UI.Control()

    static let gen = System.Random()
    [<System.NonSerialized>]
    let mutable id = System.String.Format("ws{0:x}", gen.Next().ToString())

    override this.ID
        with get () = id
        and set x = id <- x

    override this.OnLoad _ =
        this.ID <-
            ScriptManager.Find(base.Page)
                .Register(Some id, this, Shared.Metadata, Shared.Json)

    interface INode with
        member this.IsAttribute = false
        member this.Write (_, w) =
            w.Write("""<div id="{0}"></div>""", this.ID)

    [<JavaScript>]
    abstract member Body : IControlBody

    interface IControl with
        [<JavaScript>]
        member this.Body = this.Body
        member this.Id = this.ID

    member this.GetBodyNode() =
        let t = this.GetType()
        let t = if t.IsGenericType then t.GetGenericTypeDefinition() else t
        let m = t.GetProperty("Body").GetGetMethod()
        M.MethodNode (R.ReadTypeDefinition t, R.ReadMethod m)

    interface IRequiresResources with
        member this.Requires(_) =
            this.GetBodyNode() |> Seq.singleton

        member this.Encode(meta, json) =
            [this.ID, json.GetEncoder(this.GetType()).Encode this]

    override this.Render writer =
        writer.WriteLine("<div id='{0}'></div>", this.ID)

open WebSharper.JavaScript
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

/// Implements a web control based on a quotation-wrapped top-level body.
/// Use the function ClientSide to create an InlineControl.

type private FSV = Reflection.FSharpValue

[<CompiledName "FSharpInlineControl">]
type InlineControl<'T when 'T :> IControlBody>(getLocation: unit -> string, bodyAndReqs) =
    inherit Control()

    [<System.NonSerialized>]
    let getLocation = getLocation

    [<System.NonSerialized>]
    let bodyAndReqs = bodyAndReqs

    static let ctrlReq = M.TypeNode (R.ReadTypeDefinition typeof<InlineControl<IControlBody>>)

    let args = fst bodyAndReqs
    let mutable funcName = [||]

    new (elt: Expr<'T>) =

        let getLocation() =
            let (|Val|_|) e : 't option =
                match e with
                | Quotations.Patterns.Value(:? 't as v,_) -> Some v
                | _ -> None
            let l =
                elt.CustomAttributes |> Seq.tryPick (function
                    | NewTuple [ Val "DebugRange";
                                 NewTuple [ Val (file: string)
                                            Val (startLine: int)
                                            Val (startCol: int)
                                            Val (endLine: int)
                                            Val (endCol: int) ] ] ->
                        Some (sprintf "%s: %i.%i-%i.%i" file startLine startCol endLine endCol)
                    | _ -> None)
            defaultArg l "(no location)"

        let bodyAndReqs =
            let declType, meth, args, fReqs, subs =
                let elt =
                    match elt :> Expr with
                    | Coerce (e, _) -> e
                    | e -> e
                let rec get subs expr =
                    match expr with
                    | PropertyGet(None, p, args) ->
                        let m = p.GetGetMethod(true)
                        let dt = R.ReadTypeDefinition p.DeclaringType
                        let meth = R.ReadMethod m
                        dt, meth, args, [M.MethodNode (dt, meth)], subs
                    | Call(None, m, args) ->
                        let dt = R.ReadTypeDefinition m.DeclaringType
                        let meth = R.ReadMethod m
                        dt, meth, args, [M.MethodNode (dt, meth)], subs
                    | Let(var, value, body) ->
                        get (subs |> Map.add var value) body
                    | e -> failwithf "Wrong format for InlineControl at %s: expected global value or function access, got: %A" (getLocation()) e
                get Map.empty elt
            let args, argReqs =
                args
                |> List.mapi (fun i value ->
                    let rec get expr =
                        match expr with
                        | Value (v, t) ->
                            let v = match v with null -> WebSharper.Core.Json.Internal.MakeTypedNull t | _ -> v
                            v, M.TypeNode (R.ReadTypeDefinition t)
                        | TupleGet(v, i) ->
                            let v, n = get v
                            FSV.GetTupleField(v, i), n
                        | Var v when subs.ContainsKey v ->
                            get subs.[v]   
                        | _ -> failwithf "Wrong format for InlineControl at %s: argument #%i is not a literal or a local variable" (getLocation()) (i+1)
                    get value
                )
                |> List.unzip
            let args = Array.ofList args
            let reqs = ctrlReq :: fReqs @ argReqs
            args, (declType, meth, reqs)
        new InlineControl<'T> (getLocation, bodyAndReqs)

    [<JavaScript>]
    override this.Body =
        let f = Array.fold (?) JS.Window funcName
        As<Function>(f).ApplyUnsafe(null, args) :?> _

    interface IRequiresResources with
        member this.Encode(meta, json) =
            if funcName.Length = 0 then
                let declType, meth, reqs = snd bodyAndReqs
                let fail() =
                    failwithf "Error in InlineControl at %s: Couldn't find translation of method %s.%s. The method or type should have JavaScript attribute or a proxy, and the assembly needs to be compiled with WsFsc.exe" 
                        (getLocation()) declType.Value.FullName meth.Value.MethodName
                match meta.Classes.TryFind declType with
                | None -> fail()
                | Some cls ->
                    match cls.Methods.TryFind meth with
                    | Some (M.Static a, _, _) ->
                        funcName <- Array.ofList (List.rev a.Value)
                    | Some _ ->
                        failwithf "Error in InlineControl at %s: Method %s.%s must be static and not inlined"
                            (getLocation()) declType.Value.FullName meth.Value.MethodName
                    | None -> fail()
            [this.ID, json.GetEncoder(this.GetType()).Encode this]

        member this.Requires(_) =
            let _, _, reqs = snd bodyAndReqs 
            this.GetBodyNode() :: reqs |> Seq.ofList

open System
open System.Reflection
open System.Linq.Expressions

// TODO: test in arguments: needs .NET 4.5
// open System.Runtime.CompilerServices
//[<CallerFilePath; Optional>] sourceFilePath 
//[<CallerLineNumber; Optional>] sourceLineNumber
[<CompiledName "InlineControl">]
type CSharpInlineControl(elt: System.Linq.Expressions.Expression<Func<IControlBody>>) =
    inherit Control()

    [<System.NonSerialized>]
    let elt = elt

    static let ctrlReq = M.TypeNode (R.ReadTypeDefinition typeof<InlineControl<IControlBody>>)

    [<System.NonSerialized>]
    let bodyAndReqs =
        let reduce (e: Expression) = if e.CanReduce then e.Reduce() else e
        let declType, meth, args, fReqs =
            match reduce elt.Body with
            | :? MemberExpression as e ->
                match e.Member with
                | :? PropertyInfo as p ->
                    let m = p.GetGetMethod(true)
                    let dt = R.ReadTypeDefinition p.DeclaringType
                    let meth = R.ReadMethod m
                    dt, meth, [], [M.MethodNode (dt, meth)]
                | _ -> failwith "member must be a property"
            | :? MethodCallExpression as e -> 
                let m = e.Method
                let dt = R.ReadTypeDefinition m.DeclaringType
                let meth = R.ReadMethod m
                dt, meth, e.Arguments |> List.ofSeq, [M.MethodNode (dt, meth)]
            | e -> failwithf "Wrong format for InlineControl: expected global value or function access, got: %A"  e
        let args, argReqs =
            args
            |> List.mapi (fun i a -> 
                let rec get needType (a: Expression) =
                    match reduce a with
                    | :? ConstantExpression as e ->
                        let v = match e.Value with null -> WebSharper.Core.Json.Internal.MakeTypedNull e.Type | _ -> e.Value
                        v, if needType then M.TypeNode (R.ReadTypeDefinition e.Type) else M.EntryPointNode
                    | :? MemberExpression as e ->
                        let o = 
                            match e.Expression with
                            | null -> null
                            | ee -> fst (get false ee)
                        match e.Member with
                        | :? FieldInfo as f ->
                            f.GetValue(o), if needType then M.TypeNode (R.ReadTypeDefinition f.FieldType) else M.EntryPointNode
                        | :? PropertyInfo as p ->
                            if p.GetIndexParameters().Length > 0 then
                                failwithf "Wrong format for InlineControl in argument #%i, indexed property not allowed" (i+1)
                            p.GetValue(o, null), if needType then M.TypeNode (R.ReadTypeDefinition p.PropertyType) else M.EntryPointNode
                        | m -> failwithf "Wrong format for InlineControl in argument #%i, member access not allowed: %s" (i+1) (m.GetType().Name)
                    | a -> failwithf "Wrong format for InlineControl in argument #%i, expression type: %s" (i+1) (a.GetType().Name)
                get true a
            )
            |> List.unzip
        let args = Array.ofList args
        let reqs = ctrlReq :: fReqs @ argReqs
        args, (declType, meth, reqs)

    let args = fst bodyAndReqs
    let mutable funcName = [||]

    [<JavaScript>]
    override this.Body =
        let f = Array.fold (?) JS.Window funcName
        As<Function>(f).ApplyUnsafe(null, args) :?> _

    interface IRequiresResources with
        member this.Encode(meta, json) =
            if funcName.Length = 0 then
                let declType, meth, reqs = snd bodyAndReqs
                let fail() =
                    failwithf "Error in InlineControl: Couldn't find translation of method %s.%s. The method or type should have JavaScript attribute or a proxy, and the project file needs to include Zafir.CSharp.targets" 
                        declType.Value.FullName meth.Value.MethodName
                match meta.Classes.TryFind declType with
                | None -> fail()
                | Some cls ->
                    match cls.Methods.TryFind meth with
                    | Some (M.Static a, _, _) ->
                        funcName <- Array.ofList (List.rev a.Value)
                    | Some _ -> 
                        failwithf "Error in InlineControl: Method %s.%s must be static and not inlined"
                            declType.Value.FullName meth.Value.MethodName
                    | None -> fail()
            [this.ID, json.GetEncoder(this.GetType()).Encode this]

        member this.Requires(_) =
            let _, _, reqs = snd bodyAndReqs 
            this.GetBodyNode() :: reqs |> Seq.ofList

module ClientSideInternals =

    open System.Web.UI
    module M = WebSharper.Core.Metadata
    module R = WebSharper.Core.AST.Reflection
    module J = WebSharper.Core.Json
    module P = FSharp.Quotations.Patterns

    let getLocation' (q: Expr) =
        let (|Val|_|) e : 't option =
            match e with
            | Quotations.Patterns.Value(:? 't as v,_) -> Some v
            | _ -> None
        let l =
            q.CustomAttributes |> Seq.tryPick (function
                | NewTuple [ Val "DebugRange";
                             NewTuple [ Val (file: string)
                                        Val (startLine: int)
                                        Val (startCol: int)
                                        Val (endLine: int)
                                        Val (endCol: int) ] ] ->
                    Some (sprintf "%s: %i.%i-%i.%i" file startLine startCol endLine endCol)
                | _ -> None)
        defaultArg l "(no location)"

    let (|Val|_|) e : 't option =
        match e with
        | Quotations.Patterns.Value(:? 't as v,_) -> Some v
        | _ -> None

    let getLocation (q: Expr) =
        q.CustomAttributes |> Seq.tryPick (function
            | P.NewTuple [ Val "DebugRange";
                           P.NewTuple [ Val (file: string)
                                        Val (startLine: int)
                                        Val (startCol: int)
                                        Val (endLine: int)
                                        Val (endCol: int) ] ] ->
                ({
                    FileName = System.IO.Path.GetFileName(file)
                    Start = (startLine, startCol)
                    End = (endLine, endCol)
                } : WebSharper.Core.AST.SourcePos)
                |> Some
            | _ -> None)

    let rec findArgs (env: Set<string>) (setArg: string -> obj -> unit) (q: Expr) =
        match q with
        | P.ValueWithName (v, _, n) when not (env.Contains n) -> setArg n v
        | P.AddressOf q
        | P.Coerce (q, _)
        | P.FieldGet (Some q, _)
        | P.QuoteRaw q
        | P.QuoteTyped q
        | P.VarSet (_, q)
        | P.WithValue (_, _, q)
            -> findArgs env setArg q
        | P.AddressSet (q1, q2)
        | P.Application (q1, q2)
        | P.Sequential (q1, q2)
        | P.TryFinally (q1, q2)
        | P.WhileLoop (q1, q2)
            -> findArgs env setArg q1; findArgs env setArg q2
        | P.PropertyGet (q, _, qs)
        | P.Call (q, _, qs) ->
            Option.iter (findArgs env setArg) q
            List.iter (findArgs env setArg) qs
        | P.FieldSet (q1, _, q2) ->
            Option.iter (findArgs env setArg) q1; findArgs env setArg q2
        | P.ForIntegerRangeLoop (v, q1, q2, q3) ->
            findArgs env setArg q1
            findArgs env setArg q2
            findArgs (Set.add v.Name env) setArg q3
        | P.IfThenElse (q1, q2, q3)
            -> findArgs env setArg q1; findArgs env setArg q2; findArgs env setArg q3
        | P.Lambda (v, q) ->
            findArgs (Set.add v.Name env) setArg q
        | P.Let (v, q1, q2) ->
            findArgs env setArg q1
            findArgs (Set.add v.Name env) setArg q2
        | P.LetRecursive (vqs, q) ->
            let vs, qs = List.unzip vqs
            let env = (env, vs) ||> List.fold (fun env v -> Set.add v.Name env)
            List.iter (findArgs env setArg) qs
            findArgs env setArg q
        | P.NewObject (_, qs)
        | P.NewRecord (_, qs)
        | P.NewTuple qs
        | P.NewUnionCase (_, qs)
        | P.NewArray (_, qs) ->
            List.iter (findArgs env setArg) qs
        | P.NewDelegate (_, vs, q) ->
            let env = (env, vs) ||> List.fold (fun env v -> Set.add v.Name env)
            findArgs env setArg q
        | P.PropertySet (q1, _, qs, q2) ->
            Option.iter (findArgs env setArg) q1
            List.iter (findArgs env setArg) qs
            findArgs env setArg q2
        | P.TryWith (q, v1, q1, v2, q2) ->
            findArgs env setArg q
            findArgs (Set.add v1.Name env) setArg q1
            findArgs (Set.add v2.Name env) setArg q2
        | _ -> ()

    let compile (meta: M.Info) (json: J.Provider) (reqs: list<M.Node>) (q: Expr) : (obj[] * _) =
        let rec compile (reqs: list<M.Node>) (q: Expr) =
            match getLocation q with
            | Some p ->
                match meta.Quotations.TryGetValue(p) with
                | false, _ ->
                    let ex =
                        meta.Quotations.Keys
                        |> Seq.map (sprintf "  %O")
                        |> String.concat "\n"
                    failwithf "Failed to find compiled quotation at position %O\nExisting ones:\n%s" p ex
                | true, (declType, meth, argNames) ->
                    match meta.Classes.TryGetValue declType with
                    | false, _ -> failwithf "Error in Handler: Couldn't find JavaScript address for method %s.%s" declType.Value.FullName meth.Value.MethodName
                    | true, c ->
                        let argIndices = Map (argNames |> List.mapi (fun i x -> x, i))
                        let args = Array.create argNames.Length null
                        let reqs = ref (M.MethodNode (declType, meth) :: M.TypeNode declType :: reqs)
                        let setArg (name: string) (value: obj) =
                            let i = argIndices.[name]
                            if isNull args.[i] then
                                args.[i] <-
                                    match value with
                                    | :? Expr as q ->
                                        failwith "Error in Handler: Spliced expressions are not allowed"
                                        //let x, reqs' = compile !reqs q
                                        //reqs := reqs'
                                        //x
                                    | value ->
                                        let typ = value.GetType()
                                        reqs := M.TypeNode (WebSharper.Core.AST.Reflection.ReadTypeDefinition typ) :: !reqs
                                        value
                        findArgs Set.empty setArg q
                        let addr =
                            match c.Methods.TryGetValue meth with
                            | true, (M.CompiledMember.Static x, _, _) -> x.Value
                            | _ -> failwithf "Error in Handler: Couldn't find JavaScript address for method %s.%s" declType.Value.FullName meth.Value.MethodName
                        args, !reqs
            | None -> failwithf "Failed to find location of quotation: %A" q
        compile reqs q 

namespace WebSharper

[<AutoOpen>]
module WebExtensions =

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open WebSharper.Core
    open WebSharper.Web

    /// Embed the given client-side control body in a server-side control.
    /// The client-side control body must be either a module-bound or static value,
    /// or a call to a module-bound function or static method, and all arguments
    /// must be either literals or references to local variables.
    let ClientSide (e: Expr<#IControlBody>) =
        new InlineControl<_>(e)

    open ClientSideInternals

    /// Embed the given client-side control body in a server-side control.
    /// The client-side control body must be either a module-bound or static value,
    /// or a call to a module-bound function or static method, and all arguments
    /// must be either literals or references to local variables.
    let ClientSide2 ([<JavaScript>] e: Expr<#IControlBody>) =
        let (|Val|_|) e : 't option =
            match e with
            | Quotations.Patterns.Value(:? 't as v,_) -> Some v
            | _ -> None
        let pos =
            e.CustomAttributes |> Seq.tryPick (function
                | NewTuple [ Val "DebugRange";
                             NewTuple [ Val (file: string)
                                        Val (startLine: int)
                                        Val (startCol: int)
                                        Val (endLine: int)
                                        Val (endCol: int) ] ] ->
                    ({
                        FileName = System.IO.Path.GetFileName(file)
                        Start = (startLine, startCol)
                        End = (endLine, endCol)
                    } : WebSharper.Core.AST.SourcePos)
                    |> Some
                | _ -> None)
        match pos with
        | None -> failwith "Failed to find location of quotation"
        | Some p ->
            match Shared.Metadata.Quotations.TryGetValue p with
            | true, (ty, m, _) ->
                let args, deps = compile Shared.Metadata Shared.Json [] e
                new InlineControl<_>((fun () -> ""), (args, (ty, m, deps)))
            | false, _ ->
                let all =
                    Shared.Metadata.Quotations.Keys
                    |> Seq.map (sprintf "    %O")
                    |> String.concat "\n"
                failwithf "Failed to find compiled quotation at position %O\nExisting ones:\n%O" p all

    type WebSharper.Web.Context with
        member this.ClientSide([<WebSharper.JavaScript>] elt: Expr<#IControlBody>) =
            let (|Val|_|) e : 't option =
                match e with
                | Quotations.Patterns.Value(:? 't as v,_) -> Some v
                | _ -> None
            let pos =
                elt.CustomAttributes |> Seq.tryPick (function
                    | NewTuple [ Val "DebugRange";
                                 NewTuple [ Val (file: string)
                                            Val (startLine: int)
                                            Val (startCol: int)
                                            Val (endLine: int)
                                            Val (endCol: int) ] ] ->
                        ({
                            FileName = System.IO.Path.GetFileName(file)
                            Start = (startLine, startCol)
                            End = (endLine, endCol)
                        } : WebSharper.Core.AST.SourcePos)
                        |> Some
                    | _ -> None)
            match pos with
            | None -> failwith "Failed to find location of quotation"
            | Some p ->
                match this.Metadata.Quotations.TryGetValue p with
                | true, (ty, m, _) ->
                    let deps = this.Dependencies.GetDependencies [WebSharper.Core.Metadata.MethodNode(ty, m)]
                    new InlineControl<_>((fun () -> ""), ([||], (ty, m, deps)))
                | false, _ ->
                    let all =
                        this.Metadata.Quotations.Keys
                        |> Seq.map (sprintf "    %O")
                        |> String.concat "\n"
                    failwithf "Failed to find compiled quotation at position %O\nExisting ones:\n%O" p all
            :> WebSharper.Web.Control
