module ControlFlowGraph.InputStructures

open System.Collections.Generic

open ControlFlowGraph.Common

///Contains information about parser generated stuff:
///number to string mapping, token to number mapping etc.
type CfgParserSource<'TokenType> = 
    val TokenToNumber : 'TokenType -> int
    val LeftSides : array<int>
    val TokenToData : 'TokenType -> obj
    val NumToString : int -> string

    new (tokenToNumber, numToString, leftSides, tokenData) =
        {
            TokenToNumber = tokenToNumber;
            NumToString = numToString;
            LeftSides = leftSides;
            TokenToData = tokenData;
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