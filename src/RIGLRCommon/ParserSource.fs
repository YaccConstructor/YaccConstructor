namespace Yard.Generators.RIGLR

type ParserSource<'TokenType> ( table             : (int*int)[][][]
                                , tokenToNumber   : 'TokenType -> int
                                , genLiteral      : string -> int -> 'TokenType option
                                , numToString     : int -> string
                                , tokenData       : 'TokenType -> obj
                              ) = 
    
    member this.Table              = table
    member this.TokenToNumber      = tokenToNumber
    member this.GenLiteral         = genLiteral 
    member this.TokenData          = tokenData
    member this.NumToString        = numToString