module FsaHelper

open System.IO

open YC.FSA.GraphBasedFsa
open YC.FSA.FsaApproximation
open Utils

let replace origFsa matchFsa replaceFsa =
    let equalSmbl x y = (fst x) = (fst y)
    let getChar x = 
        match x with
        | Smbl(y, _) -> y
        | _ -> failwith "Unexpected symb in alphabet of FSA!"
    let newSmb x =  Smbl(x, Unchecked.defaultof<_>)
    FSA.Replace (origFsa, matchFsa, replaceFsa, '~', '^', getChar, newSmb, equalSmbl)

/// Checks if the language accepted by fsa a1 is a sublanguage of the language accepted by fsa a2
let isSubAutomaton (a1: FSA<char * Position<int>>) (a2: FSA<char * Position<int>>) =
    let a2Complement = a2.Complementation
    let equalSmbl x y = (fst x) = (fst y)
    let intersection = FSA.Intersection (a1, a2Complement, equalSmbl)
    intersection.IsEdgesEmpty && intersection.IsVerticesEmpty

let toDot (fsa: FSA<char * Position<int>>) path =
    fsa.PrintToDOT (path, (fun p -> sprintf "%c" (fst p)))

let toDebugDot (fsa: FSA<char * Position<int>>) name =
    let path = Path.Combine (myDebugFolderPath, name + ".dot")
    toDot fsa path