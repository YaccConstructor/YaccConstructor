module ControlFlowGraph.InputStructures

open System.Collections.Generic

open ControlFlowGraph.Common

open QuickGraph.FSA.GraphBasedFsa
open QuickGraph.FSA.FsaApproximation

///Contains information about lexer and parser generated stuff:
///number to string mapping, token to number mapping etc.
type GeneratedStuffSource<'TokenType, 'BackReference when 'BackReference : equality> = 
    val TokenToNumber : 'TokenType -> int
    val LeftSides : array<int>
    val TokenToData : 'TokenType -> obj
    val NumToString : int -> string
    val FsaInfo : FsaParams<char, char * Position<'BackReference>> option

    new (tokenToNumber, numToString, leftSides, tokenData, fsaInfo) =
        {
            TokenToNumber = tokenToNumber;
            NumToString = numToString;
            LeftSides = leftSides;
            TokenToData = tokenData;
            FsaInfo = Some fsaInfo;
        }

    new (tokenToNumber, numToString, leftSides, tokenData) =
        {
            TokenToNumber = tokenToNumber;
            NumToString = numToString;
            LeftSides = leftSides;
            TokenToData = tokenData;
            FsaInfo = None;
        }

    

    member this.TokenToString = this.TokenToNumber >> this.NumToString
        
///Contains information about language:
///non-terminal to block type mapping, 
///keywords to int mapping etc
type LanguageSource = 
    val NodeToType : IDictionary<string, BlockType>
    val KeywordToInt : IDictionary<Keyword, int>
    val IsVariable : int -> bool

    new (nodeToType, keywordToInt) = 
        {
            NodeToType = nodeToType; 
            KeywordToInt = keywordToInt;
            IsVariable = fun _ -> false;
        }

    new (nodeToType, keywordToInt, isVariable) = 
        {
            NodeToType = nodeToType; 
            KeywordToInt = keywordToInt;
            IsVariable = isVariable;
        }

    ///int -> statement type
    member this.GetTempIfDict() = 
        let dict = new Dictionary<_, _>()
        
        if this.KeywordToInt.ContainsKey Keyword.IF
        then dict.[this.KeywordToInt.[Keyword.IF]] <- IfStatement
        
        if this.KeywordToInt.ContainsKey Keyword.THEN
        then dict.[this.KeywordToInt.[Keyword.THEN]] <- ThenStatement
        
        if this.KeywordToInt.ContainsKey Keyword.ELSE
        then dict.[this.KeywordToInt.[Keyword.ELSE]] <- ElseStatement

        dict