// ILComparators.fs contains functions for IL compare
//
//  Copyright 2012 Semen Grigorev <rsdpisuy@gmail.com>
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

module Yard.Core.ILComparators

open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition


let GrammarEqualsWithoutLineNumbers (g1:Grammar.t<Source.t,Source.t>) (g2:Grammar.t<Source.t, Source.t>) =
    let srcEquals (a:Source.t) (b:Source.t) =
        if (fst a = fst b) then true
        else printfn "bad %A %A" a b; false

    let srcOptEquals a b =
        match a,b with
        | Some(sa), Some(sb) -> srcEquals sa sb
        | None, None -> true
        | _ -> printfn "badOpt %A %A" a b; false

    let argsAreEqual a b =
        List.length a = List.length b 
        &&  List.forall2 srcEquals a b

    let rec ilTreeEqualsWithoutLineNumbers il1 il2 =
        let rec reduceSeq = function
            | PSeq ([{omit = false; binding = None; checker = None; rule = r}], None) ->
                reduceSeq r
            | x -> x
        //printfn "compare\n%A\n\n%A\n=======================\n" (reduceSeq il1) (reduceSeq il2)
        match (reduceSeq il1, reduceSeq il2) with
        | PSeq(elems1, ac1), PSeq(elems2, ac2) -> 
            List.length elems1 = List.length elems2 &&
                List.zip elems1 elems2 
                |> List.forall 
                    (fun (elem1, elem2) ->
                        srcOptEquals elem1.binding elem2.binding && srcOptEquals elem1.checker elem2.checker &&
                            elem1.omit = elem2.omit && ilTreeEqualsWithoutLineNumbers elem1.rule elem2.rule
                    )
        | PAlt(left1, right1), PAlt(left2, right2) -> 
            ilTreeEqualsWithoutLineNumbers left1 left2 && ilTreeEqualsWithoutLineNumbers right1 right2
        | PToken(t1), PToken(t2) -> srcEquals t1 t2
        | PRef(r1, args1), PRef(r2, args2) -> srcEquals r1 r2 && srcOptEquals args1 args2
        | PMany(t1), PMany(t2) -> ilTreeEqualsWithoutLineNumbers t1 t2
        | PSome(t1), PSome(t2) -> ilTreeEqualsWithoutLineNumbers t1 t2
        | POpt(t1), POpt(t2) -> ilTreeEqualsWithoutLineNumbers t1 t2
        | PMetaRef(r1, arg1, marg1), PMetaRef(r2, arg2, marg2) -> 
            srcEquals r1 r2 && srcOptEquals arg1 arg2 && 
                List.length marg1 = List.length marg2 && List.forall2 ilTreeEqualsWithoutLineNumbers marg1 marg2
        | PLiteral(s1), PLiteral(s2) -> srcEquals s1 s2
        | _ -> false

    List.forall2  
        (fun (rule1:Rule.t<Source.t, Source.t>) (rule2:Rule.t<Source.t, Source.t>) ->
            rule1._public = rule2._public &&
            argsAreEqual rule1.args rule2.args &&
            ilTreeEqualsWithoutLineNumbers rule1.body rule2.body &&
            List.forall2 srcEquals rule1.metaArgs rule2.metaArgs &&
            rule1.name = rule2.name
        ) g1 g2

