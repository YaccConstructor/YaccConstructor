//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.


module Yard.Utils.StructClass


(*
  For Trinity:
    amount of files limited: 131071 files
    line's length limited: 131071 symbols
    file's length limited: 1073741823 lines (1 billion lines)

  For Pair:
    amount of files limited: 131071 files
    absolute offset limited: 140736414482432 symbols (140 trillion 736 billon)  
*)

[<Measure>] type id
[<Measure>] type symbol
[<Measure>] type line

let _id = 1<id>
let _symbol = 1<symbol>
let _symbolL = 1L<symbol>
let _line = 1<line>

[<Struct>]
type Trinity = 
    val Id : int<id>
    val Column : int<symbol>
    val Line : int<line>
    new (i, c, l) = { Id = i; Column = c; Line = l }

[<Struct>]
type Pair = 
    val Id : int<id>
    val AbsoluteOffset : int64<symbol>
    new (i : int<id>, o : int64<symbol>)  = { Id = i; AbsoluteOffset = o }

