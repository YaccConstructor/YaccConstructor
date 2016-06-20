module Util
    
    open System.Collections.Generic

    type NonTerminal = NonTerminal of string

    type ComplexRule = { 
        Head: NonTerminal;
        LeftTail: NonTerminal;
        RightTail: NonTerminal; 
        probability: double;
    }

    type SimpleRule = {
        Head: NonTerminal;
        Tail: char;
        probability: double;
    } 

    type EpsRule =  {
        Head: NonTerminal;
        probability: double;
    } 

    type RulesHolder(complexRules: Dictionary<(NonTerminal * NonTerminal), (NonTerminal * double) list>,
                     simpleRules: Dictionary<char, (NonTerminal * double) list>,
                     epsilonRules: NonTerminal list)  =  
                                         
        member this.SimpleTails = simpleRules.Keys
        member this.IsSimpleTail = simpleRules.ContainsKey
        member this.HeadBySimpleTail c  = simpleRules.Item c

        member this.ComplexTails = complexRules.Keys
        member this.IsComplexTail = complexRules.ContainsKey
        member this.HeadByComplexTail c = complexRules.Item c