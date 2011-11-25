//  Copyright 2010-2011 by Konstantin Ulitin
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace Yard.Frontends.YardFrontend

open Yard.Core

type YardFrontend() = 
    interface IFrontend with
        member this.Name = "YardFrontend"
        member this.ParseGrammar t = 
            match t with
            | (:? System.String as s) -> Main.ParseFile s
            | _ -> IL.Definition.empty
        member this.ProductionTypes = List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>) |> List.map (fun unionCase -> unionCase.Name)
    end

// For testing switch to Console App and then switch back to Class Library
module Run = 
     //let filename = @"..\..\..\..\Tests\Basic\test_include\test_include_main.yrd" 
    let filename = @"..\..\..\..\Tests\RACC\test_arithm_glr\test_arithm_glr.yrd" 
    printf "%A\n" <| Yard.Frontends.YardFrontend.Main.ParseFile filename