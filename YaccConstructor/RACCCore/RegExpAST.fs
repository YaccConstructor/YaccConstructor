// RegExpAST.fs contains types and functions for trace manipulations and 
// trace to tree transformation
//
//  Copyright 2009,2010 Semen Grigorev <rsdpisuy@gmail.com>
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

namespace Yard.Generators.RACCGenerator


type REAST = 
   | RESeq of List<REAST>
   | REClosure of List<REAST>
   | REAlt of Option<REAST>*Option<REAST>
   | RELeaf of obj

type RegExpAST() = 
    class
        let isCorrectPair l r =
            match l,r with
            | FATrace l , FATrace r ->
                match l,r with
                | TSmbS x, TSmbE y 
                | TSeqS x, TSeqE y 
                | TAlt1S x, TAlt1E y
                | TAlt2S x, TAlt2E y 
                | TClsS x, TClsE y  when x=y -> true
                | _                          -> false
            | _ -> false

        let rec verifyTrace trace =
            match trace with
            | hd::tl ->                 
                match hd with
                | FATrace (TSmbS x) ->                                         
                    match tl with 
                    | _hd::tl ->                   
                        match _hd with
                        | FATrace (TSmbE y)-> true,tl
                        | _             -> false,[]
                    | _   -> false,[]
                | FATrace (TAlt1S x)->
                    let r,_tl = verifyTrace tl 
                    if r
                    then
                        match _tl with
                        | FATrace (TAlt1E y)::tl -> verifyTrace tl
                        | _          -> false,[]
                    else false,[]
                | FATrace (TAlt2S x) ->
                    let r,_tl = verifyTrace tl
                    if r
                    then
                        match _tl with
                        | FATrace (TAlt2E y)::tl -> verifyTrace tl
                        | _          -> false,[]
                    else false,[]

                | FATrace (TSeqS x) ->
                    let rec inner tl =
                        let r,_tl = verifyTrace tl
                        if r
                        then
                            match _tl with
                            | FATrace (TSeqE y)::tl -> true, tl
                            | _          -> inner _tl                    
                        else false,[]
                    inner tl

                 | FATrace (TClsS x) ->
                    let rec inner tl =
                        let r,_tl = verifyTrace tl
                        if r
                        then
                            match _tl with
                            | FATrace (TClsE y)::tl -> true, tl
                            | _          -> inner _tl                    
                        else false,[]
                    inner tl
                | _    -> false,[]
                    
            | []     -> true,[]

        let rec buildREAST trace values =
            match trace with
            | hd::tl ->                 
                match hd with
                | FATrace (TSmbS x)->                                         
                    match tl with 
                    | _hd::tl ->                   
                        match _hd with
                        | FATrace (TSmbE y) -> List.head values |> RELeaf, tl, List.tail values
                        | _             -> RELeaf null,[],[]
                    | _   -> RELeaf null,[],[]
                | FATrace (TAlt1S x) ->
                    let r,_tl,_val = buildREAST tl values
                    match _tl with
                    | FATrace (TAlt1E y)::tl -> REAlt (Some(r),None), tl, _val
                    | _          -> RELeaf null,[],[]                
                | FATrace (TAlt2S x)->
                    let r,_tl,_val = buildREAST tl values
                    match _tl with
                    | FATrace (TAlt2E y)::tl -> REAlt (None,Some(r)), tl, _val
                    | _          -> RELeaf null,[],[]   

                | FATrace (TSeqS x)->
                    let rec inner buf tl vals =
                        let r,_tl,_val = buildREAST tl vals                        
                        match _tl with
                        | FATrace (TSeqE y)::tl -> r::buf |> List.rev |> RESeq, tl, _val
                        | _          -> inner (r::buf) _tl _val                                  
                    inner [] tl values

                | FATrace (TClsS x)->
                    let rec inner buf tl vals =
                        let r,_tl,_val = buildREAST tl vals                        
                        match _tl with
                        | FATrace (TClsE y)::tl -> r::buf |> List.rev |> REClosure, tl, _val
                        | _          -> inner (r::buf) _tl _val                                  
                    inner [] tl values 
                
                | _    -> RELeaf null,[],[]
                    
            | []     -> RELeaf null,[],[]

        let rec buildCorrectTrace trace =
            match trace with
            | hd1::hd2::tl1 -> 
                Set.fold
                    (fun buf partTrace ->
                        match partTrace with
                        | t1::tl2 ->                             
                            Set.filter 
                                (fun elt -> List.rev elt |> List.head |> fun x -> isCorrectPair x t1)
                                hd2
                            |> Set.map 
                                (fun header -> header @ partTrace)

                        | []     -> Set.empty)
                    Set.empty
                    hd1
                |> fun x -> x::tl1 |> buildCorrectTrace
            | hd::[] -> hd
            | []     -> Set.empty                    

        member self.BuilCorrectTrace trace = List.rev trace |> buildCorrectTrace

        member self.BuildREAST trace values = buildREAST trace values
    end