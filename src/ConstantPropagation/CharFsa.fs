module CharFsa

open System.Collections.Generic
open Microsoft.FSharp.Collections

open Yard.Utils.StructClass
open QuickGraph.FSA.GraphBasedFsa
open JetBrains.ReSharper.Psi.CSharp.Tree

type FsaState<'Lit> = char * Position<'Lit>

let alphabet = 
    let lst = 
        [
            ['a' .. 'z'];
            ['A' .. 'Z'];
            ['0' .. '9'];
            [
                '*'; '/'; '+'; '-'; '='; '>'; '<';
                '{'; '}';  '('; ')'; '['; ']';
                '.'; ','; ':'; ';'; '!'; '?';
                '"'; '''; 
                ' '; '\t'; '\n';
                '@'; '&'; '%'; '$'; '#'; '№'
            ];
        ]
        |> List.concat

    HashSet(lst)

let private getCharMetEpsMsg = "getChar met Eps symbol"
let symbolsAreEqual (s1: FsaState<_>) (s2: FsaState<_>) = fst s1 = fst s2
let getChar = function Smbl(ch, p) -> ch | Eps -> failwith getCharMetEpsMsg
let newSymbol x =  Smbl(x, Unchecked.defaultof<_>)

let separatorChar1 = '~'
let separatorChar2 = '^'

let charFsaParams = {
    Alphabet = alphabet 
    NewSymbol = newSymbol
    GetChar = getChar
    SymbolsAreEqual = symbolsAreEqual
    SeparatorSmbl1 = separatorChar1
    SeparatorSmbl2 = separatorChar2 }

let toDot (fsa: FSA<FsaState<_>>) path =
    let stateToString (st: char * 'b) =
        let ch = fst st
        let p = snd st
        sprintf "%c" ch
    fsa.PrintToDOT (path, stateToString)