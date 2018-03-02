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

namespace WebSharper

open System
open System.Linq
open System.Collections
open System.Collections.Generic
open WebSharper.Core
open WebSharper.JavaScript

open FSharp.Linq

[<Proxy(typeof<QuerySource<_,_>>)>]
type internal QuerySourceProxy<'T, 'Q> [<Inline "$source">] (source: IEnumerable<'T>) =

    [<Inline "$this">]
    member this.Source = source
                                
[<Proxy(typeof<QueryBuilder>)>]
type internal QueryBuilderProxy() =
    [<Inline>]
    member this.All(source: QuerySource<'T, 'Q>, predicate: 'T -> bool) =
        Seq.forall predicate source.Source

    [<Inline>]
    member this.AverageBy(source: QuerySource<'T, 'Q>, projection: 'T -> 'TValue) =
        Seq.averageBy projection source.Source

    member this.AverageByNullable(source: QuerySource<'T, 'Q>, projection: 'T -> Nullable<'TValue>) =
        source.Source 
        |> Seq.averageBy (fun x -> 
            let y = projection x 
            if y.HasValue then y.Value else As 0
        )

    [<Inline>]
    member this.Contains(source: QuerySource<'T, 'Q>, key: 'T) =
        Seq.contains key source.Source

    [<Inline>]
    member this.Count(source: QuerySource<'T, 'Q>) =
        Seq.length source.Source

    [<Inline>]
    member this.Distinct(source: QuerySource<'T, 'Q>) =
        Seq.distinct source.Source |> QuerySource

    [<Inline>]
    member this.ExactlyOne(source: QuerySource<'T, 'Q>) =
        Seq.exactlyOne source.Source
     
    [<Inline>]
    member this.ExactlyOneOrDefault(source: QuerySource<'T, 'Q>) =
        source.Source.SingleOrDefault()

    [<Inline>]
    member this.Exists(source: QuerySource<'T, 'Q>, predicate: 'T -> bool) =
        Seq.exists predicate source.Source
     
    [<Inline>]
    member this.Find(source: QuerySource<'T, 'Q>, predicate: 'T -> bool) =
        Seq.find predicate source.Source

    member this.For(source: QuerySource<'T, 'Q>, body: 'T -> QuerySource<'TResult, 'Q2>) =
        Seq.collect (fun x -> (body x).Source) source.Source |> QuerySource
     
    [<Inline>]
    member this.GroupBy(source: QuerySource<'T, 'Q>, keySelector: 'T -> 'TKey) =
        source.Source.GroupBy(fun x -> keySelector x) |> QuerySource

    member this.GroupJoin
      (
        outerSource: QuerySource<'TOuter, 'Q>, innerSource: QuerySource<'TInner, 'Q>, 
        outerKeySelector: 'TOuter -> 'TKey, innerKeySelector: 'TInner -> 'TKey,
        resultSelector: 'TOuter -> 'TInner -> 'TResult
      ) =
        outerSource.Source.GroupJoin(
            innerSource.Source, 
            (fun x -> outerKeySelector x), 
            (fun x -> innerKeySelector x), 
            (fun x y -> resultSelector x y)
        ) |> QuerySource

    [<Inline>]
    member this.GroupValBy(source: QuerySource<'T, 'Q>, resultSelector: 'T -> 'TValue, keySelector: 'T -> 'TKey) =
        source.Source.GroupBy(fun x -> keySelector x, fun x -> resultSelector x) |> QuerySource

    [<Inline>]
    member this.Head(source: QuerySource<'T, 'Q>) =
        Seq.head source.Source

    [<Inline>]
    member this.HeadOrDefault(source: QuerySource<'T, 'Q>) =
        source.Source.FirstOrDefault()
    
    member this.Join
      (
        outerSource: QuerySource<'TOuter, 'Q>, innerSource: QuerySource<'TInner, 'Q>, 
        outerKeySelector: 'TOuter -> 'TKey, innerKeySelector: 'TInner -> 'TKey,
        resultSelector: 'TOuter -> 'TInner -> 'TResult
      ) =
        outerSource.Source.Join(
            innerSource.Source, 
            (fun x -> outerKeySelector x), 
            (fun x -> innerKeySelector x), 
            (fun x y -> resultSelector x y)
        ) |> QuerySource

    [<Inline>]
    member this.Last(source: QuerySource<'T, 'Q>) =
        Seq.tail source.Source
 
    [<Inline>]
    member this.LastOrDefault(source: QuerySource<'T, 'Q>) =
        source.Source.LastOrDefault()

[<WebSharper.Proxy
    "Microsoft.FSharp.Core.ExtraTopLevelOperators, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private ExtraTopLevelOperatorsQueryProxy =
    
    let query = QueryBuilderProxy()
