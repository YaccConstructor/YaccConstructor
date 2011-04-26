//  Module BuildAST contains:
//  - function, which replaces all action code with code that constructs AST in own format.
//
//  Copyright 2009-2011 Konstantin Ulitin
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

module BuildAST

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production

open System.Collections.Generic

(* You need to add following code in grammar header *)
type AST<'token> =
    | Node of string * AST<'token> list
    | Leaf of string * 'token

(* End of header *)

let seqify = function
    | PSeq(x, y) -> PSeq(x, y)
    | production -> PSeq([{new elem<Source.t, Source.t> with omit=false and rule=production and binding=None and checker=None}], None)

let printSeqProduction binding = function
    | POpt(x) -> sprintf "%s" binding
    | PToken(s,_) -> sprintf "Some(Leaf(\"%s\", %s))" s binding
    //| PSome(x) | PMany (x) | PRef((r,_),_) | PAlt(_)-> sprintf "Some(%s)" binding
    | _ -> sprintf "Some(%s)" binding

let rec _buildAST ruleName (production: t<Source.t, Source.t>) = 
    match production with
    | PSeq(elements, _) -> PSeq(elements |> List.mapi (fun i elem -> 
            if elem.omit then { elem with binding=None ; rule=(_buildAST (ruleName+"_inner") elem.rule) } else { elem with binding=Some((sprintf "_S%d" i),(0,0)) ; rule=(_buildAST (ruleName+"_inner") elem.rule) }
        ), 
        Some(sprintf "Node(\"%s\", [%s] |> List.choose (fun x -> x) )" ruleName (elements |> List.mapi (fun i elem -> (i, elem)) |> List.choose (fun (i, elem) -> if elem.omit then None else Some(printSeqProduction (sprintf "_S%d" i) elem.rule)) |> String.concat "; "), (0,0)))
    | PAlt(left, right) -> PAlt(_buildAST (ruleName+"_inner") left, _buildAST (ruleName+"_inner") right)
    | PSome(x) -> PSome(_buildAST (ruleName+"_inner") x)
    | PMany(x) -> PMany(_buildAST (ruleName+"_inner") x)
    | POpt(x) -> POpt(_buildAST (ruleName+"_inner") x)
    | x -> x

let buildAST (ruleList: Rule.t<Source.t, Source.t> list) = 
    ruleList |> List.map (fun rule -> {rule with body=(_buildAST rule.name rule.body) } )

type BuildAST() = 
    interface IConvertion with
        member this.Name = "BuildAST"
        member this.ConvertList ruleList = buildAST ruleList
        member this.EliminatedProductionTypes = [""]
    end

