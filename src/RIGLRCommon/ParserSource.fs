namespace Yard.Generators.RIGLR

type ParserSource<'TokenType> ( table             : (int*int)[][][]
                                , tokenToNumber   : 'TokenType -> int
                                , genLiteral      : string -> int -> 'TokenType option
                                , numToString     : int -> string
                                , tokenData       : 'TokenType -> obj
                                , leftSide        : int[]
                                , startRule       : int
                                , eofIndex        : int
                              ) = 
    
    member this.Table              = table
    member this.TokenToNumber      = tokenToNumber
    member this.GenLiteral         = genLiteral 
    member this.TokenData          = tokenData
    member this.NumToString        = numToString
    member this.LeftSide           = leftSide
    member this.StartRule          = startRule
    member this.EofIndex           = eofIndex