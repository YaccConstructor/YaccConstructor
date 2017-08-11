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

namespace Yard.Core

open Yard.Core.IL

[<AbstractClass>]
type Conversion() = //as this =
    abstract Name : string
    abstract ConvertGrammar : Grammar.t<Source.t, Source.t> * string[] -> Grammar.t<Source.t, Source.t>
    abstract ConvertGrammar : Grammar.t<Source.t, Source.t>          -> Grammar.t<Source.t, Source.t>
    default this.ConvertGrammar grammar = this.ConvertGrammar (grammar,[||]) 