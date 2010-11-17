// RegExpAST.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators._RACCGenerator


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
                | TSmbS, TSmbE 
                | TSeqS, TSeqE 
                | TAlt1S, TAlt1E 
                | TAlt2S, TAlt2E 
                | TClsS, TClsE -> true
                | _            -> false
            | _ -> false

        let rec verifyTrace trace =
            match trace with
            | hd::tl ->                 
                match hd with
                | FATrace TSmbS ->                                         
                    match tl with 
                    | _hd::tl ->                   
                        match _hd with
                        | FATrace TSmbE -> true,tl
                        | _             -> false,[]
                    | _   -> false,[]
                | FATrace TAlt1S ->
                    let r,_tl = verifyTrace tl 
                    if r
                    then
                        match _tl with
                        | FATrace TAlt1E::tl -> verifyTrace tl
                        | _          -> false,[]
                    else false,[]
                | FATrace TAlt2S ->
                    let r,_tl = verifyTrace tl
                    if r
                    then
                        match _tl with
                        | FATrace TAlt2E::tl -> verifyTrace tl
                        | _          -> false,[]
                    else false,[]

                | FATrace TSeqS ->
                    let rec inner tl =
                        let r,_tl = verifyTrace tl
                        if r
                        then
                            match _tl with
                            | FATrace TSeqE::tl -> true, tl
                            | _          -> inner _tl                    
                        else false,[]
                    inner tl
                | _    -> false,[]
                    
            | []     -> true,[]
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

        let buildREAST trace =
            match trace with
            | 1 -> 1

        member self.BuilCorrectTrace trace = List.rev trace |> buildCorrectTrace  |> Set.filter (verifyTrace >> fst)
    end