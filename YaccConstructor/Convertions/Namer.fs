// module Namer contains: 
//  constants and functions for naming of
//  different items, created by Yard (during gammar transformations)
//
//  Copyright 2009,2010 Ilia Chemodanov
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

module Yard.Core.Namer

open IL
open Production

(** prefix for all items created by Yard *)
let private withPrefix s = "yard_" + s

(** global variable for number of current generated rule *)
let curNum = ref 0

let resetRuleEnumerator () = curNum := 0

let private genYardName n (l, c) = (sprintf "%s_%d" (withPrefix n) !curNum), (l, c)

let createName ((n:string), (l, c)) =     
    let addPrefix = 
      try
        let yardPrefix = withPrefix ""
        if (n.Substring (0, System.Math.Min(n.Length, yardPrefix.Length))) = yardPrefix 
        then fun x->x 
        else withPrefix
      with (*Invalid_argument*) _ -> withPrefix
    in sprintf "%s_%d" (addPrefix n) !curNum, (l, c)

/// Create new Source.t item with name, consisting of yard prefix, specified middle
///    and postfix, generated as regularly incrementing number
let createNewName name = 
    incr curNum;
    createName name

module Names =
 begin
//sig 
//  (** metarule name for option: "item?" *)
//  val opt:string 
//  (** metarule name for some: "item+" *)
//  val some:string 
//  (** metarule name for many: "item*" *)
//  val many:string 
//  (** rule prefix for sequence *)
//  val seq:string 
//  (** rule prefix for alternative *)
//  val alt:string 
//  (** rule prefix for predicate *)
//  val predicate:string
//end 

   let x = withPrefix

   let opt  = x "option"
   let some = x "nlist"
   let many = x "list"
   let seq  = x "seq"
   let alt  = x "alt"
   let predicate = x "predicate"
   let brackets = x "exp_brackets"
end 

(** returns true if given name is metarule name for EBNF *)
let isEBNFmeta name = 
    name = Names.opt || name = Names.some || name = Names.many

//let getMetaRuleName rName = 
//    try
//      let ind = String.rindex rName '_' in
//      let rName' = rName.Substring(0, ind) 
//      in
//        if (isEBNFmeta rName') then rName'
//        else begin
//          let ind' = (rName'.IndexOf '_') + 1
//          in rName'.Substring(ind', (rName'.Length - ind'))
//        end
//    with Not_found -> rName

/// formal parameter name in metarules for EBNF clauses
let getItem = "item"

let getItemBind = withPrefix getItem

/// Does name have prefix 'item'
let isItem (name:string) = 
    let len = getItem.Length in
    if  name.Length >= len then
        name.Substring(0, len) = getItem 
    else false
    
(** names specific for Elkhound, *)

//(** there is special nonterminal for empty alternative: empty *)
//let getEmptyAltName = "empty"

(** name of parameter in function "keep" *)
let getKeepParamName = withPrefix "val"

let headBind = withPrefix "h"

let tailBind = withPrefix "t"

let oneElemListAction = sprintf " [%s] "

let addToListAction = sprintf " %s::%s " headBind tailBind

let someAction = sprintf " Some %s "

let noneAction = " None "

/// prefix for Elkhound bindings
let withElkPrefix x = "_elk_" + x

/// token type (need for using bindings with tokens)
let withTokenPrefix (* token name *) = 
    sprintf "'%stoken_%s" (withPrefix "") (* token name *)

/// type of semantic value
let createTypeName (* ruleName *) =  sprintf "'%stype_%s" (withPrefix "") (* ruleName *)

/// returns file name for tokens
let createTknFileName fname = System.IO.Path.GetFileNameWithoutExtension(fname) |> sprintf "tokens_%s.tok"

(*
/// Create new name for rule
let createRuleName name =  sprintf "'%srule_%s_%d" (withPrefix "") (fst name) (incr curNum; !curNum)

/// Create new name for argument
let createArgName =  sprintf "'%sarg_%d" (withPrefix "") (incr curNum; !curNum)
*)
/// returns token type for literal
let createLiteralToken (* number *) = sprintf "LITERAL_%d" (* number *)

/// returns token name for EOF (End Of File)
let getEofTokenName = (withPrefix "EOF").ToUpper()
    
(* end of module Namer *)


