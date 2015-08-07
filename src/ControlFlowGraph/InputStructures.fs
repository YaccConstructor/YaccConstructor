module ControlFlowGraph.InputStructures

open System.Collections.Generic

open ControlFlowGraph.Common

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
        if this.KeywordToInt.ContainsKey IF
        then dict.[this.KeywordToInt.[IF]] <- IfStatement
        if this.KeywordToInt.ContainsKey THEN
        then dict.[this.KeywordToInt.[THEN]] <- ThenStatement
        if this.KeywordToInt.ContainsKey ELSE
        then dict.[this.KeywordToInt.[ELSE]] <- ElseStatement

        dict