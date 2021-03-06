﻿// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace WebSharper.Sitelets.Tests

open WebSharper
open WebSharper.Testing
open WebSharper.Sitelets
open PerformanceTests
open RouterOperators

[<JavaScript>]
module ClientServerTests =

    let ShiftedRouter = 
        Router.Shift "perf-tests" <| Router.Infer<Action>()

    let Tests apiBaseUri =
        let parse router p =
            Route.FromUrl(p) |> Router.Parse router    
        let parseHash router p =
            Route.FromHash(p, true) |> Router.Parse router
        let rUnit = rRoot |> Router.MapTo ()

        let writeAction (v: Action) =
            try sprintf "%A" v
            with _ -> "Failed writing action value"

        let hasMethodOrBody (v: Action) =
            match v with
            | UPost _ 
            | UPut _
            | UPost2 _
            | UJsonInput _
            | UJsonInt _
            | UFormData _
            | UMultiFormData _
                -> true
            | _ -> false

        TestCategory "Sitelets Client-server routing" {
            Test "compatibility tests" {
                let! serverResults = GetTestValues()
                let testValues = serverResults |> Array.map (fun (testValue, _, _) -> testValue)
                let serverLinks = serverResults |> Array.map (fun (_, serverLink, _) -> serverLink)
                let serverParsed = serverResults |> Array.map (fun (_, _, testValue) -> testValue)
                let clientLinks = testValues |> Array.map ShiftedRouter.Link
                let clientParsed = clientLinks |> Array.map (fun l -> Router.Parse ShiftedRouter (Route.FromUrl l))
                forEach (Array.zip3 testValues serverLinks clientLinks) (fun (v, s, c) ->
                    Do { equalMsg s c ("Generated link equal: " + writeAction v) }
                )
                forEach (Array.zip testValues serverParsed) (fun (v, p) ->
                    if hasMethodOrBody v then Do.Zero() else
                    Do { equalMsg p (Some v) ("Parsing back on the server: " + writeAction v) }
                )
                forEach (Array.zip testValues clientParsed) (fun (v, p) ->
                    if hasMethodOrBody v then Do.Zero() else
                    Do { equalMsg p (Some v) ("Parsing back on the client: " + writeAction v) }
                )
            }

            Test "Router.Ajax" {
                let! serverResults = GetTestValues()
                let! ajaxResults =
                    async {
                        let arr = ResizeArray()
                        for testValue, _, _ in serverResults do
                            try
                                do! Expect testValue
                                let! res = Router.Ajax ShiftedRouter testValue
                                arr.Add (box res)
                            with e ->
                                arr.Add (box e.StackTrace)
                        return arr.ToArray()
                    }
                let expectedResults =
                    serverResults |> Array.map (fun (_, serverLink, _) ->
                        box serverLink
                    )
                forEach (Array.zip ajaxResults expectedResults) (fun (r, v) ->
                    Do { equalMsg r v (sprintf "Ajax call for: " + string v) }
                )
            }

            Test "Router primitives" {
                equal (Router.Link rUnit ()) "/"
                equal (parse rUnit "/") (Some ())
                equal (Router.Link rInt 2) "/2"
                equal (parse rInt "/2") (Some 2)
            }

            Test "Router.HashLink" {
                equal (Router.HashLink rUnit ()) "#/"
                equal (parseHash rUnit "") (Some ())
                equal (parseHash rUnit "#") (Some ())
                equal (parseHash rUnit "#/") (Some ())
                equal (Router.HashLink rInt 2) "#/2"
                equal (parseHash rInt "#/2") (Some 2)                
            }

            Test "Router combinator" {
                let! testValuesAndServerLinks = CombinatorTests.GetTestValues()
                let testValuesAndClientLinks =
                    testValuesAndServerLinks |> Array.map (fun (testValue, _) ->
                        testValue, CombinatorTests.constructed.Link testValue    
                    )
                equal testValuesAndServerLinks testValuesAndClientLinks
            }
        }

    let RunTests apiBaseUri =
        Runner.RunTests [|
            Tests apiBaseUri
        |]

