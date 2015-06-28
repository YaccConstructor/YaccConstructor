module CharFsa

open System.Collections.Generic
open Microsoft.FSharp.Collections

open YC.FSA.FsaApproximation
open YC.FSA.GraphBasedFsa
open FsaHelper
open JetBrains.ReSharper.Psi.CSharp.Tree

type FsaState<'Lit> = char * Position<'Lit>

let alphabet = 
    let lst = 
        [
            'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n';
            'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z';
            'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N';
            'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z';
            '*'; ','; ' '; '-'; '('; ')'; '@'; ';'; '+'; '/'; '"'; '{'; '}'; '=';
        ]
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