//   Copyright 2013 YaccConstructor Software Foundation
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

module Yard.Core.ConversionsManager

open Yard.Core
open Yard.Core.IL

type ConversionsManager () as this =
    inherit Yard.Core.Manager.Manager<Conversion>()
    let apply_Conversion (convNameWithParams:string) (ilTree:Definition.t<Source.t,Source.t>)  = 
      {ilTree
        with grammar =
                let parameters = convNameWithParams.Split(' ')
                //printfn "Conversion: %s" convNameWithParams
                if parameters.Length = 0 then failwith "Missing Conversion name"
                else
                    match this.Component parameters.[0] with 
                    | Some conv -> conv.ConvertGrammar (ilTree.grammar, parameters.[1..parameters.Length - 1])
                    | None -> failwith <| "Conversion not found: " + parameters.[0]
      }
    member  self.ApplyConversion convNameWithParams ilTree =
        apply_Conversion convNameWithParams ilTree
     