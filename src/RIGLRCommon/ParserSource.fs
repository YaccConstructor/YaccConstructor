namespace Yard.Generators.RIGLR

type ParserSource<'TokenType> ( table             : (int*int)[][][]
                                , tokenToNumber   : 'TokenType -> int
                                , genLiteral      : string -> int -> 'TokenType option
                                , numToString     : int -> string
                                , tokenData       : 'TokenType -> obj
                                , rules           : int[]
                                , rulesStart      : int[]  
                                , leftSide        : int[]
                                , startRule       : int
                                , eofIndex        : int
                                , popStates       : Set<int>
                                , finalState      : int list
                              ) = 
    let length =
        let res = Array.zeroCreate (rulesStart.Length - 1)
        for i = 0 to res.Length - 1 do
            res.[i] <- rulesStart.[i + 1] - rulesStart.[i]
        res
    let _rules = Array.zeroCreate length.Length
    do 
        for i = 0 to length.Length - 1 do
        _rules.[i] <- Array.zeroCreate length.[i]
        for j = 0 to length.[i] - 1 do
            _rules.[i].[j] <- rules.[rulesStart.[i] + j]

    member this.Table              = table
    member this.TokenToNumber      = tokenToNumber
    member this.GenLiteral         = genLiteral 
    member this.TokenData          = tokenData
    member this.Rules              = _rules
    member this.NumToString        = numToString
    member this.LeftSide           = leftSide
    member this.StartRule          = startRule
    member this.EofIndex           = eofIndex
    member this.PopStates          = popStates
    member this.FinalState         = finalState