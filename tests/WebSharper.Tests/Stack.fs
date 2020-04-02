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

module WebSharper.Tests.Stack

open WebSharper
open WebSharper.Testing
type private Stack<'T> = System.Collections.Generic.Stack<'T>

[<JavaScript>]
let Tests =

    TestCategory "Stack" {

        Test "new" {
            let s = Stack<int>()
            equal s.Count 0
            let s2 = Stack([ 1 .. 5])
            equal s2.Count 5
            equal (s2.Pop()) 5
            equal s2.Count 4
        }

        Test "Push" {
            let s = Stack<int>()
            s.Push 1
            s.Push 2
            equal (s.ToArray()) [| 2; 1 |]
        }

        Test "Pop" {
            let s = Stack<int>()
            s.Push 1
            s.Push 2
            equal (s.Pop()) 2
            equal (s.ToArray()) [|1|]
        }

    }
