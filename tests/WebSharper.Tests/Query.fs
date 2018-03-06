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

module WebSharper.Tests.Query

open System
open WebSharper
open WebSharper.Testing
open FSharp.Linq

[<JavaScript>]
let Tests =
    let s = seq { 1 .. 10 } 
    
    TestCategory "Query" {

        Test "All" {
            let q =
                query {
                    for x in s do
                    all (x > 0)
                }
            isTrue q 
        }

        Test "AverageBy" {
            let q =
                query {
                    for x in s do
                    averageBy (float x)
                }
            equal q 5.5
        }

        Test "AverageByNullable" {
            let n = Seq.singleton (Nullable()) 
            let s = s |> Seq.map Nullable |> Seq.append n
            let q =
                query {
                    for x in s do
                    averageByNullable (Nullable.float x)
                }
            equal q (Nullable 5.5)
            let q2 =
                query {
                    for x in n do
                    averageByNullable (Nullable.float x)
                }
            equal q2 (Nullable())
        }

        Test "Contains" {
            let q =
                query {
                    for x in s do
                    contains 11
                }
            isFalse q
        }

        Test "Count" {
            let q =
                query {
                    for x in s do
                    count
                }
            equal q 10
        }

        Test "Distinct" {
            let s2 = s |> Seq.append (Seq.singleton 1) 
            let q =
                query {
                    for x in s2 do
                    distinct
                }
            equal (Array.ofSeq q) (Array.ofSeq s)
        }
    }
