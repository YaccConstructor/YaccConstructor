//this file was generated by GNESCC
//source grammar:../../../Tests/GNESCC/regexp/simple/alt/alt.yrd
//date:06.12.2011 22:03:00

module GNESCC.Actions_alt

open Yard.Generators.GNESCCGenerator

let getUnmatched x expectedType =
    "Unexpected type of node\nType " + x.ToString() + " is not expected in this position\n" + expectedType + " was expected." |> failwith

let value x = (x:>Lexer_alt.MyLexeme).MValue

let s0 expr = 
    let inner  = 
        match expr with
        | REAlt(Some(x), None) -> 
            let yardLAltAction expr = 
                match expr with
                | RESeq [x0] -> 
                    let (p) =
                        let yardElemAction expr = 
                            match expr with
                            | RELeaf tPLUS -> tPLUS :?> 'a
                            | x -> getUnmatched x "RELeaf"

                        yardElemAction(x0)
                    ("Detected: " + (value p))
                | x -> getUnmatched x "RESeq"

            yardLAltAction x 
        | REAlt(None, Some(x)) -> 
            let yardRAltAction expr = 
                match expr with
                | RESeq [x0] -> 
                    let (m) =
                        let yardElemAction expr = 
                            match expr with
                            | RELeaf tMULT -> tMULT :?> 'a
                            | x -> getUnmatched x "RELeaf"

                        yardElemAction(x0)
                    ("Detected: " + (value m))
                | x -> getUnmatched x "RESeq"

            yardRAltAction x 
        | x -> getUnmatched x "REAlt"
    box (inner)

let ruleToAction = dict [|(1,s0)|]

