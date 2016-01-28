 
module YC.FST.AbstractLexing.LiteralsLexer

open Microsoft.FSharp.Collections
open QuickGraph.FST.GraphBasedFst
open QuickGraph.FSA.GraphBasedFsa
open YC.FST.AbstractLexing.Interpreter
open AbstractAnalysis.Common
open AbstractParser.Tokens
open System.Collections.Generic

let fstLexer () = 
   let startState = ResizeArray.singleton 0
   let finishState = ResizeArray.singleton 65535
   let transitions = new ResizeArray<_>()
   transitions.Add(0, (Smbl 65535, Eps), 65535)
   transitions.Add(0, (Smbl 39, Eps), 1)
   transitions.Add(1, (Smbl 39, Eps), 1)
   transitions.Add(1, (Smbl 48, Eps), 2)
   transitions.Add(1, (Smbl 49, Eps), 2)
   transitions.Add(1, (Smbl 50, Eps), 2)
   transitions.Add(1, (Smbl 51, Eps), 2)
   transitions.Add(1, (Smbl 52, Eps), 2)
   transitions.Add(1, (Smbl 53, Eps), 2)
   transitions.Add(1, (Smbl 54, Eps), 2)
   transitions.Add(1, (Smbl 55, Eps), 2)
   transitions.Add(1, (Smbl 56, Eps), 2)
   transitions.Add(1, (Smbl 57, Eps), 2)
   transitions.Add(1, (Smbl 65535, Eps), 65535)
   transitions.Add(2, (Smbl 39, Eps), 3)
   transitions.Add(2, (Smbl 48, Eps), 4)
   transitions.Add(2, (Smbl 49, Eps), 4)
   transitions.Add(2, (Smbl 50, Eps), 4)
   transitions.Add(2, (Smbl 51, Eps), 4)
   transitions.Add(2, (Smbl 52, Eps), 4)
   transitions.Add(2, (Smbl 53, Eps), 4)
   transitions.Add(2, (Smbl 54, Eps), 4)
   transitions.Add(2, (Smbl 55, Eps), 4)
   transitions.Add(2, (Smbl 56, Eps), 4)
   transitions.Add(2, (Smbl 57, Eps), 4)
   transitions.Add(2, (Smbl 65535, Eps), 65535)
   transitions.Add(3, (Smbl 39, Smbl 0), 1)
   transitions.Add(3, (Smbl 65535, Smbl 0), 65535)
   transitions.Add(4, (Smbl 39, Eps), 3)
   transitions.Add(4, (Smbl 48, Eps), 4)
   transitions.Add(4, (Smbl 49, Eps), 4)
   transitions.Add(4, (Smbl 50, Eps), 4)
   transitions.Add(4, (Smbl 51, Eps), 4)
   transitions.Add(4, (Smbl 52, Eps), 4)
   transitions.Add(4, (Smbl 53, Eps), 4)
   transitions.Add(4, (Smbl 54, Eps), 4)
   transitions.Add(4, (Smbl 55, Eps), 4)
   transitions.Add(4, (Smbl 56, Eps), 4)
   transitions.Add(4, (Smbl 57, Eps), 4)
   transitions.Add(4, (Smbl 65535, Eps), 65535)
   new FST<_,_>(startState, finishState, transitions)

let actions () =
   [|

      (fun (gr : FSA<_>) ->
                                   LITERAL(gr) |> Some );

   |]


let alphabet () = 
 new HashSet<_>([| Smbl 65535; Smbl 39; Smbl 48; Smbl 49; Smbl 50; Smbl 51; Smbl 52; Smbl 53; Smbl 54; Smbl 55; Smbl 56; Smbl 57;|])

let tokenize eof approximation = Tokenize (fstLexer()) (actions()) (alphabet()) eof approximation
