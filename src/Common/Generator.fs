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
open Mono.Addins

[<assembly:AddinRoot ("YaccConstructor", "1.0")>]
do()

[<AbstractClass>]
[<TypeExtensionPoint>]
type Generator() = //as this =
    abstract Name : string
    abstract Generate : Definition.t<Source.t,Source.t> -> obj
    abstract Generate : Definition.t<Source.t,Source.t> * string -> obj
    default this.Generate(grammar, string) = this.Generate grammar
    abstract Constraints : Constraint []
//    interface Yard.Core.Manager.IComponent with
//        member self.Name : string =  this.Name