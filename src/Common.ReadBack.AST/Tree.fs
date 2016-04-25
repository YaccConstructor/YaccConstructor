module Yard.Generators.RNGLR.ReadBack.Tree


type Tree<'TokenType>(ast : Ast<'TokenType>, leftSide : int[]) =
    member this.Ast = ast
    member this.LeftSide = leftSide


and Ast<'TokenType> =
    | Node of int * AstEdge<'TokenType>[]
    | Leaf of 'TokenType option //None for epsilons

and AstEdge<'TokenType>(nfaDest : int, astLabel : Ast<'TokenType>) =
    member this.NfaDest = nfaDest
    member this.SubTree = astLabel