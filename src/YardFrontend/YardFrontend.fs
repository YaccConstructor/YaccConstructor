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

namespace Yard.Frontends.YardFrontend

open Mono.Addins
open Yard.Core

[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()

[<Extension>]
type YardFrontend() = 
    inherit Frontend()
        override this.Name = "YardFrontend"
        override this.ParseGrammar t = 
            match t with
            | (:? System.String as s) -> 
                let inliner = new Conversions.ExpandInline.ReplaceInline()                
                let g = Main.ParseFile s
                { g with grammar = inliner.ConvertGrammar g.grammar }
            | _ -> failwithf "File name expected as argumnet for YardFrontend.ParseGrammar, but got: %A" t
        override this.ProductionTypes =
            List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>)
            |> List.map (fun unionCase -> unionCase.Name)