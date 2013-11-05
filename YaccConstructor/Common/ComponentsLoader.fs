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

module Yard.Core.ComponentsLoader

open System
open System.IO
open Microsoft.FSharp.Collections

let private loadByType (desiredType:Type) =
    let basePath = AppDomain.CurrentDomain.BaseDirectory
    let assemblies =
        System.IO.Directory.GetFiles(basePath,"*.dll")
        |> Seq.map Path.GetFileNameWithoutExtension
        |> Seq.choose (fun x -> try Reflection.Assembly.Load x |> Some with _ -> None)
   
    assemblies
    |> Seq.collect (fun assembly -> try assembly.GetTypes() with _ -> printfn "Assambly %s could not de loaded." assembly.FullName; [||] )
    |> Seq.filter (fun _type -> desiredType.IsAssignableFrom(_type))

let private isRealClass (cls:Type) =
    not cls.IsAbstract
    && not cls.IsGenericTypeDefinition
    && not cls.IsInterface

let LoadComponents (desiredType:Type) =
    loadByType desiredType
    |> ResizeArray.ofSeq
    |> ResizeArray.filter isRealClass
    |> ResizeArray.choose (fun x -> try  Activator.CreateInstance x |> Some with _ -> None)
