 
module SimpleTest.Lexer

open Microsoft.FSharp.Collections
open QuickGraph.FST.GraphBasedFst
open QuickGraph.FSA.GraphBasedFsa
open YC.FST.AbstractLexing.Interpreter
open AbstractAnalysis.Common
open SimpleTest.Parser
open System.Collections.Generic
//open YC.SDK.CommonInterfaces

let fstLexer () = 
   let startState = ResizeArray.singleton 0
   let finishState = ResizeArray.singleton 65535
   let transitions = new ResizeArray<_>()
   transitions.AddRange([|(0, (Smbl 65535, Eps), 65535); (0, (Smbl 9, Eps), 1); (0, (Smbl 10, Eps), 1); (0, (Smbl 13, Eps), 1); (0, (Smbl 32, Eps), 1); (0, (Smbl 59, Eps), 10); (0, (Smbl 65, Eps), 2); (0, (Smbl 66, Eps), 3); (0, (Smbl 67, Eps), 4); (0, (Smbl 68, Eps), 5); (0, (Smbl 69, Eps), 6); (0, (Smbl 70, Eps), 7); (0, (Smbl 71, Eps), 8); (0, (Smbl 72, Eps), 9); (1, (Smbl 9, Smbl 0), 1); (1, (Smbl 10, Smbl 0), 1); (1, (Smbl 13, Smbl 0), 1); (1, (Smbl 32, Smbl 0), 1); (1, (Smbl 59, Smbl 0), 10); (1, (Smbl 65, Smbl 0), 2); (1, (Smbl 66, Smbl 0), 3); (1, (Smbl 67, Smbl 0), 4); (1, (Smbl 68, Smbl 0), 5); (1, (Smbl 69, Smbl 0), 6); (1, (Smbl 70, Smbl 0), 7); (1, (Smbl 71, Smbl 0), 8); (1, (Smbl 72, Smbl 0), 9); (1, (Smbl 65535, Smbl 0), 65535); (10, (Smbl 9, Smbl 9), 1); (10, (Smbl 10, Smbl 9), 1); (10, (Smbl 13, Smbl 9), 1); (10, (Smbl 32, Smbl 9), 1); (10, (Smbl 59, Smbl 9), 10); (10, (Smbl 65, Smbl 9), 2); (10, (Smbl 66, Smbl 9), 3); (10, (Smbl 67, Smbl 9), 4); (10, (Smbl 68, Smbl 9), 5); (10, (Smbl 69, Smbl 9), 6); (10, (Smbl 70, Smbl 9), 7); (10, (Smbl 71, Smbl 9), 8); (10, (Smbl 72, Smbl 9), 9); (10, (Smbl 65535, Smbl 9), 65535); (2, (Smbl 9, Smbl 1), 1); (2, (Smbl 10, Smbl 1), 1); (2, (Smbl 13, Smbl 1), 1); (2, (Smbl 32, Smbl 1), 1); (2, (Smbl 59, Smbl 1), 10); (2, (Smbl 65, Smbl 1), 2); (2, (Smbl 66, Smbl 1), 3); (2, (Smbl 67, Smbl 1), 4); (2, (Smbl 68, Smbl 1), 5); (2, (Smbl 69, Smbl 1), 6); (2, (Smbl 70, Smbl 1), 7); (2, (Smbl 71, Smbl 1), 8); (2, (Smbl 72, Smbl 1), 9); (2, (Smbl 65535, Smbl 1), 65535); (3, (Smbl 9, Smbl 2), 1); (3, (Smbl 10, Smbl 2), 1); (3, (Smbl 13, Smbl 2), 1); (3, (Smbl 32, Smbl 2), 1); (3, (Smbl 59, Smbl 2), 10); (3, (Smbl 65, Smbl 2), 2); (3, (Smbl 66, Smbl 2), 3); (3, (Smbl 67, Smbl 2), 4); (3, (Smbl 68, Smbl 2), 5); (3, (Smbl 69, Smbl 2), 6); (3, (Smbl 70, Smbl 2), 7); (3, (Smbl 71, Smbl 2), 8); (3, (Smbl 72, Smbl 2), 9); (3, (Smbl 65535, Smbl 2), 65535); (4, (Smbl 9, Smbl 3), 1); (4, (Smbl 10, Smbl 3), 1); (4, (Smbl 13, Smbl 3), 1); (4, (Smbl 32, Smbl 3), 1); (4, (Smbl 59, Smbl 3), 10); (4, (Smbl 65, Smbl 3), 2); (4, (Smbl 66, Smbl 3), 3); (4, (Smbl 67, Smbl 3), 4); (4, (Smbl 68, Smbl 3), 5); (4, (Smbl 69, Smbl 3), 6); (4, (Smbl 70, Smbl 3), 7); (4, (Smbl 71, Smbl 3), 8); (4, (Smbl 72, Smbl 3), 9); (4, (Smbl 65535, Smbl 3), 65535); (5, (Smbl 9, Smbl 4), 1); (5, (Smbl 10, Smbl 4), 1); (5, (Smbl 13, Smbl 4), 1); (5, (Smbl 32, Smbl 4), 1); (5, (Smbl 59, Smbl 4), 10); (5, (Smbl 65, Smbl 4), 2); (5, (Smbl 66, Smbl 4), 3); (5, (Smbl 67, Smbl 4), 4); (5, (Smbl 68, Smbl 4), 5); (5, (Smbl 69, Smbl 4), 6); (5, (Smbl 70, Smbl 4), 7); (5, (Smbl 71, Smbl 4), 8); (5, (Smbl 72, Smbl 4), 9); (5, (Smbl 65535, Smbl 4), 65535); (6, (Smbl 9, Smbl 5), 1); (6, (Smbl 10, Smbl 5), 1); (6, (Smbl 13, Smbl 5), 1); (6, (Smbl 32, Smbl 5), 1); (6, (Smbl 59, Smbl 5), 10); (6, (Smbl 65, Smbl 5), 2); (6, (Smbl 66, Smbl 5), 3); (6, (Smbl 67, Smbl 5), 4); (6, (Smbl 68, Smbl 5), 5); (6, (Smbl 69, Smbl 5), 6); (6, (Smbl 70, Smbl 5), 7); (6, (Smbl 71, Smbl 5), 8); (6, (Smbl 72, Smbl 5), 9); (6, (Smbl 65535, Smbl 5), 65535); (7, (Smbl 9, Smbl 6), 1); (7, (Smbl 10, Smbl 6), 1); (7, (Smbl 13, Smbl 6), 1); (7, (Smbl 32, Smbl 6), 1); (7, (Smbl 59, Smbl 6), 10); (7, (Smbl 65, Smbl 6), 2); (7, (Smbl 66, Smbl 6), 3); (7, (Smbl 67, Smbl 6), 4); (7, (Smbl 68, Smbl 6), 5); (7, (Smbl 69, Smbl 6), 6); (7, (Smbl 70, Smbl 6), 7); (7, (Smbl 71, Smbl 6), 8); (7, (Smbl 72, Smbl 6), 9); (7, (Smbl 65535, Smbl 6), 65535); (8, (Smbl 9, Smbl 7), 1); (8, (Smbl 10, Smbl 7), 1); (8, (Smbl 13, Smbl 7), 1); (8, (Smbl 32, Smbl 7), 1); (8, (Smbl 59, Smbl 7), 10); (8, (Smbl 65, Smbl 7), 2); (8, (Smbl 66, Smbl 7), 3); (8, (Smbl 67, Smbl 7), 4); (8, (Smbl 68, Smbl 7), 5); (8, (Smbl 69, Smbl 7), 6); (8, (Smbl 70, Smbl 7), 7); (8, (Smbl 71, Smbl 7), 8); (8, (Smbl 72, Smbl 7), 9); (8, (Smbl 65535, Smbl 7), 65535); (9, (Smbl 9, Smbl 8), 1); (9, (Smbl 10, Smbl 8), 1); (9, (Smbl 13, Smbl 8), 1); (9, (Smbl 32, Smbl 8), 1); (9, (Smbl 59, Smbl 8), 10); (9, (Smbl 65, Smbl 8), 2); (9, (Smbl 66, Smbl 8), 3); (9, (Smbl 67, Smbl 8), 4); (9, (Smbl 68, Smbl 8), 5); (9, (Smbl 69, Smbl 8), 6); (9, (Smbl 70, Smbl 8), 7); (9, (Smbl 71, Smbl 8), 8); (9, (Smbl 72, Smbl 8), 9); (9, (Smbl 65535, Smbl 8), 65535)|])
   new FST<_,_>(startState, finishState, transitions)

let actions () =
   [|

      (fun (gr : FSA<_>) ->
                              None );
      (fun (gr : FSA<_>) ->
                       A(gr) |> Some );
      (fun (gr : FSA<_>) ->
                       B(gr) |> Some );
      (fun (gr : FSA<_>) ->
                       C(gr) |> Some );
      (fun (gr : FSA<_>) ->
                       D(gr) |> Some );
      (fun (gr : FSA<_>) ->
                       E(gr) |> Some );
      (fun (gr : FSA<_>) ->
                       F(gr) |> Some );
      (fun (gr : FSA<_>) ->
                       G(gr) |> Some );
      (fun (gr : FSA<_>) ->
                       H(gr) |> Some );
      (fun (gr : FSA<_>) ->
                       SEMICOLON(gr) |> Some );

   |]


let alphabet () = 
 new HashSet<_>([| Smbl 65535; Smbl 9; Smbl 10; Smbl 13; Smbl 32; Smbl 59; Smbl 65; Smbl 66; Smbl 67; Smbl 68; Smbl 69; Smbl 70; Smbl 71; Smbl 72;|])

let tokenize eof approximation = Tokenize (fstLexer()) (actions()) (alphabet()) eof approximation
