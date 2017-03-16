namespace AbstractAnalysis.Common

open QuickGraph
open System.Runtime.CompilerServices

[<Measure>] type token

[<Measure>] type gssVertex
[<Measure>] type nodeMeasure
[<Measure>] type positionInInput
[<Measure>] type positionInGrammar
[<Measure>] type length
[<Measure>] type leftPosition
[<Measure>] type extension

type IParserInput =
    abstract member InitialPositions: array<int<positionInInput>>    
    abstract member ForAllOutgoingEdges: int<positionInInput> -> (int<token> -> int<positionInInput> -> unit) -> unit