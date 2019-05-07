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

namespace YC.Core

open YC.Core.IL

[<AbstractClass>]
type Conversion() = //as this =
    abstract Name : string
    abstract ConvertGrammar : Grammar<Source, Source> * string[] -> Grammar<Source, Source>
    abstract ConvertGrammar : Grammar<Source, Source>            -> Grammar<Source, Source>
    default this.ConvertGrammar grammar = this.ConvertGrammar (grammar,[||])

type Constraint(name : string, checker : Grammar<Source, Source> -> bool, conv : Conversion, ?args : string[]) =
    member this.Name = name
    member this.Conversion = conv
    member this.Fix grammar =
        match args with
        | None -> conv.ConvertGrammar grammar
        | Some args -> conv.ConvertGrammar (grammar, args)
    member this.Check grammar = checker grammar

[<AbstractClass>]
type Frontend() = //as this =
    abstract Name : string
    abstract ParseGrammar : obj -> Definition<Source, Source>
    abstract ParseGrammarFromStr : string -> Definition<Source, Source>
    abstract ProductionTypes : string list
    
[<AbstractClass>]
type Generator() = //as this =
    abstract Name : string
    abstract Generate : Definition<Source, Source> * bool -> obj      
    abstract Generate : Definition<Source, Source> * bool * string -> obj
//    abstract Generate : Definition.t<Source,Source> -> obj
//    default this.Generate(grammar) = this.Generate(grammar,true)
//    abstract Generate : Definition.t<Source,Source> * string -> obj  
//    default this.Generate(grammar, str) = this.Generate(grammar,true, str)
    abstract Constraints : Constraint []    