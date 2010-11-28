//  IronyFrontend.fs
//
//  Copyright 2010 Konstantin Ulitin <ulitin.k@gmail.com>
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
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

namespace Yard.Frontends.IronyFrontend

open Yard.Core

type IronyFrontend() = 
    interface IFrontend with
        member this.Name = "IronyFrontend"
        member this.ParseGrammar t = 
            match t with
            | :? Irony.Parsing.Grammar as g -> 
                {info = {fileName = ""};
                head = None;
                grammar = Converter.Convert g;
                foot = None;}
            | _ -> IL.Definition.empty
        member this.ProductionTypes = List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>) |> List.map (fun unionCase -> unionCase.Name)
    end
