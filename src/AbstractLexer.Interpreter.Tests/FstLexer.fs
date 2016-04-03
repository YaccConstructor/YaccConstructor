module YC.FST.AbstractLexing.FstLexer

open Microsoft.FSharp.Collections
open QuickGraph.FST.GraphBasedFst
open QuickGraph.FSA.GraphBasedFsa
open YC.FST.AbstractLexing.Interpreter
open AbstractAnalysis.Common
open AbstractParser.Tokens

let fstLexer () = 
    let startState = ResizeArray.singleton 0
    let finishState = ResizeArray.singleton 65535
    let transitions = new ResizeArray<_>()
    transitions.Add(0, (Smbl (char 65535), Eps), 65535)
    transitions.Add(0, (Smbl '*', Eps), 2)
    transitions.Add(0, (Smbl '+', Eps), 1)
    transitions.Add(2, (Smbl '*', Eps), 3)
    transitions.Add(2, (Smbl '+', Smbl 2), 1)
    transitions.Add(2, (Smbl (char 65535), Smbl 2), 65535)
    transitions.Add(1, (Smbl '*', Smbl 0), 2)
    transitions.Add(1, (Smbl '+', Smbl 0), 1)
    transitions.Add(1, (Smbl (char 65535), Smbl 0), 65535)
    transitions.Add(3, (Smbl '*', Smbl 1), 2)
    transitions.Add(3, (Smbl '+', Smbl 1), 1)
    transitions.Add(3, (Smbl (char 65535), Smbl 1), 65535)
    new FST<_,_>(startState, finishState, transitions)

let actions () =
    [|
        (fun (gr : FSA<_>) ->
                        PLUS(gr) |> Some );
        (fun (gr : FSA<_>) ->
                        POW(gr) |> Some );
        (fun (gr : FSA<_>) ->
                        MULT(gr) |> Some );

    |] 

let tokenize eof approximation = Tokenize (fstLexer()) (actions()) eof approximation
