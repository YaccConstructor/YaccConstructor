//  TableInterpretator.fs contains AST interpreter which  use for in action code calculation
//
//  Copyright 2009, 2010, 2011 Semen Grigorev <rsdpisuy@gmail.com>
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

namespace Yard.Generators.GNESCCGenerator

open Yard.Generators.GNESCCGenerator.AST

type Status =
    | Success of obj
    | DelByFilter
    | Fail of string
    
module ASTInterpretator = 
    let rec lInterp 
        (ruleToActon:System.Collections.Generic.IDictionary<_,_>)
        (ruleToRegexp:System.Collections.Generic.IDictionary<_,_>) tree = 
        let reast = RegExpAST()
        match tree with        
        | Node (childs, name, trace) ->
            List.map (fun x -> lInterp ruleToActon ruleToRegexp x) childs
            |> fun x -> List.map2 (fun ch x -> match ch with Node(_,name,_) | Leaf(name,_) -> string name, x) childs x
            |> ruleToRegexp.[name] //(traceCache.[trace] |> Set.minElement |> reast.BuildREAST)
            |> ruleToActon.[name]
//             fun x -> 
//                   match x with 
//                   | (t,_,_) -> ruleToActon.[name] t                  
        | Leaf (name, value)        -> box value            

    let interp ruleToActon ruleToRegexp (tree(*:AST<ILexeme,_,List<_>>*)) = 
        try
            Success(lInterp ruleToActon ruleToRegexp tree)
        with 
        | Constants.CheckerFalse -> DelByFilter
        | e -> Fail e.Message



        
