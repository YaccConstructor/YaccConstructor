 
module YC.FST.AbstractLexing.LiteralsLexer

open Microsoft.FSharp.Collections
open YC.FST.GraphBasedFst
open YC.FST.AbstractLexing.Interpreter
open AbstractAnalysis.Common
open AbstractParser.Tokens
open System.Collections.Generic

let fstLexer () = 
   let startState = ResizeArray.singleton 0
   let finishState = ResizeArray.singleton 65535
   let transitions = new ResizeArray<_>()
   transitions.Add(0, new EdgeLbl<_,_>(Smbl (char 65535), Eps), 65535)
   transitions.Add(0, new EdgeLbl<_,_>(Smbl ''', Eps), 1)
   transitions.Add(1, new EdgeLbl<_,_>(Smbl ''', Eps), 1)
   transitions.Add(1, new EdgeLbl<_,_>(Smbl '0', Eps), 2)
   transitions.Add(1, new EdgeLbl<_,_>(Smbl '1', Eps), 2)
   transitions.Add(1, new EdgeLbl<_,_>(Smbl '2', Eps), 2)
   transitions.Add(1, new EdgeLbl<_,_>(Smbl '3', Eps), 2)
   transitions.Add(1, new EdgeLbl<_,_>(Smbl '4', Eps), 2)
   transitions.Add(1, new EdgeLbl<_,_>(Smbl '5', Eps), 2)
   transitions.Add(1, new EdgeLbl<_,_>(Smbl '6', Eps), 2)
   transitions.Add(1, new EdgeLbl<_,_>(Smbl '7', Eps), 2)
   transitions.Add(1, new EdgeLbl<_,_>(Smbl '8', Eps), 2)
   transitions.Add(1, new EdgeLbl<_,_>(Smbl '9', Eps), 2)
   transitions.Add(1, new EdgeLbl<_,_>(Smbl (char 65535), Eps), 65535)
   transitions.Add(2, new EdgeLbl<_,_>(Smbl ''', Eps), 3)
   transitions.Add(2, new EdgeLbl<_,_>(Smbl '0', Eps), 4)
   transitions.Add(2, new EdgeLbl<_,_>(Smbl '1', Eps), 4)
   transitions.Add(2, new EdgeLbl<_,_>(Smbl '2', Eps), 4)
   transitions.Add(2, new EdgeLbl<_,_>(Smbl '3', Eps), 4)
   transitions.Add(2, new EdgeLbl<_,_>(Smbl '4', Eps), 4)
   transitions.Add(2, new EdgeLbl<_,_>(Smbl '5', Eps), 4)
   transitions.Add(2, new EdgeLbl<_,_>(Smbl '6', Eps), 4)
   transitions.Add(2, new EdgeLbl<_,_>(Smbl '7', Eps), 4)
   transitions.Add(2, new EdgeLbl<_,_>(Smbl '8', Eps), 4)
   transitions.Add(2, new EdgeLbl<_,_>(Smbl '9', Eps), 4)
   transitions.Add(2, new EdgeLbl<_,_>(Smbl (char 65535), Eps), 65535)
   transitions.Add(3, new EdgeLbl<_,_>(Smbl ''', Smbl 0), 1)
   transitions.Add(3, new EdgeLbl<_,_>(Smbl (char 65535), Smbl 0), 65535)
   transitions.Add(4, new EdgeLbl<_,_>(Smbl ''', Eps), 3)
   transitions.Add(4, new EdgeLbl<_,_>(Smbl '0', Eps), 4)
   transitions.Add(4, new EdgeLbl<_,_>(Smbl '1', Eps), 4)
   transitions.Add(4, new EdgeLbl<_,_>(Smbl '2', Eps), 4)
   transitions.Add(4, new EdgeLbl<_,_>(Smbl '3', Eps), 4)
   transitions.Add(4, new EdgeLbl<_,_>(Smbl '4', Eps), 4)
   transitions.Add(4, new EdgeLbl<_,_>(Smbl '5', Eps), 4)
   transitions.Add(4, new EdgeLbl<_,_>(Smbl '6', Eps), 4)
   transitions.Add(4, new EdgeLbl<_,_>(Smbl '7', Eps), 4)
   transitions.Add(4, new EdgeLbl<_,_>(Smbl '8', Eps), 4)
   transitions.Add(4, new EdgeLbl<_,_>(Smbl '9', Eps), 4)
   transitions.Add(4, new EdgeLbl<_,_>(Smbl (char 65535), Eps), 65535)
   new FST<_,_>(startState, finishState, transitions)

let actions () =
   [|

      (fun (gr : GraphTokenValue<_>) ->
                                   LITERAL(gr) |> Some );

   |]


let alphabet () = 
 new HashSet<_>([| Smbl (char 65535); Smbl '''; Smbl '0'; Smbl '1'; Smbl '2'; Smbl '3'; Smbl '4'; Smbl '5'; Smbl '6'; Smbl '7'; Smbl '8'; Smbl '9';|])

let tokenize eof approximation = Tokenize (fstLexer()) (actions()) (alphabet()) eof approximation
