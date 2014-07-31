module YC.FST.AbstractLexing.FstLexer

open Microsoft.FSharp.Collections
open YC.FST.GraphBasedFst

let fstLexer () = 
    let startState = ResizeArray.singleton 0
    let finishState = ResizeArray.singleton 65535
    let transitions = new ResizeArray<_>()
    transitions.Add(0, new EdgeLbl<_,_>(Smbl (char 65535), Eps), 65535)
    transitions.Add(0, new EdgeLbl<_,_>(Smbl '*', Eps), 2)
    transitions.Add(0, new EdgeLbl<_,_>(Smbl '+', Eps), 1)
    transitions.Add(2, new EdgeLbl<_,_>(Smbl '*', Eps), 3)
    transitions.Add(2, new EdgeLbl<_,_>(Smbl '+', Smbl 2), 1)
    transitions.Add(2, new EdgeLbl<_,_>(Smbl (char 65535), Smbl 2), 65535)
    transitions.Add(1, new EdgeLbl<_,_>(Smbl '*', Smbl 0), 2)
    transitions.Add(1, new EdgeLbl<_,_>(Smbl '+', Smbl 0), 1)
    transitions.Add(1, new EdgeLbl<_,_>(Smbl (char 65535), Smbl 0), 65535)
    transitions.Add(3, new EdgeLbl<_,_>(Smbl '*', Smbl 1), 2)
    transitions.Add(3, new EdgeLbl<_,_>(Smbl '+', Smbl 1), 1)
    transitions.Add(3, new EdgeLbl<_,_>(Smbl (char 65535), Smbl 1), 65535)
    new FST<_,_>(startState, finishState, transitions)