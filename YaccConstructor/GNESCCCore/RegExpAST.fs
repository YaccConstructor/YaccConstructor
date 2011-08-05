// RegExpAST.fs contains types and functions for trace manipulations and 
// trace to tree transformation
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


type REAST = 
   | RESeq     of List<REAST>
   | REClosure of List<REAST>
   | REOpt     of Option<REAST>
   | REAlt     of Option<REAST>*Option<REAST>
   | RELeaf    of obj

type RegExpAST() = 
    class
        let isCorrectPair l r =
            match l,r with                            
            | TSmbS x, TSmbE y 
            | TSeqS x, TSeqE y 
            | TAlt1S x, TAlt1E y
            | TAlt2S x, TAlt2E y 
            | TOptS x, TOptE y 
            | TClsS x, TClsE y  when x=y -> true
            | _                          -> false            

        let rec buildREAST trace values =
            match trace with
            | hd::tl ->                 
                match hd with
                | TSmbS _ ->                                         
                    match tl with 
                    | _hd::tl ->                   
                        match _hd with
                        | TSmbE _ -> List.head values |> RELeaf, tl, List.tail values
                        | _       -> RELeaf null,[],[]
                    | _   -> RELeaf null,[],[]
                | TAlt1S _ ->
                    let r,_tl,_val = buildREAST tl values
                    match _tl with
                    | TAlt1E _ :: tl -> REAlt (Some(r),None), tl, _val
                    | _              -> RELeaf null,[],[]                
                | TAlt2S _ ->
                    let r,_tl,_val = buildREAST tl values
                    match _tl with
                    | TAlt2E _ :: tl -> REAlt (None,Some(r)), tl, _val
                    | _              -> RELeaf null,[],[]   

                | TSeqS _ ->
                    let rec inner buf tl vals =
                        let r,_tl,_val = buildREAST tl vals                        
                        match _tl with
                        | TSeqE _ :: tl -> r::buf |> List.rev |> RESeq, tl, _val
                        | _             -> inner (r::buf) _tl _val                                  
                    inner [] tl values

                | TClsS _ ->
                    let rec inner buf tl vals =
                        match tl with
                        | TClsE _ :: tl -> buf |> List.rev |> REClosure, tl, vals
                        | _ ->
                            let r,_tl,_val = buildREAST tl vals                        
                            match _tl with                        
                            | TClsE _ :: tl -> r::buf |> List.rev |> REClosure, tl, _val
                            | _             -> inner (r::buf) _tl _val                                  
                    inner [] tl values 

                | TOptS _ ->                    
                    match tl with
                    | TOptE y :: tl -> REOpt None, tl, values
                    | _ -> let r,_tl,_val = buildREAST tl values                            
                           match _tl with                        
                           | TOptE _ :: tl -> REOpt (Some r), tl, _val
                           | _             -> RELeaf null,[],[]                    
                
                | _    -> RELeaf null,[],[]
                    
            | []  -> RELeaf null,[],[]
        
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