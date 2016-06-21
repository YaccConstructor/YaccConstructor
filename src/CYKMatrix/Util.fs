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
        member this.HeadsBySimpleTail c = 
            if simpleRules.ContainsKey c
            then simpleRules.Item c
            else []

        member this.ComplexTails = complexRules.Keys
        member this.IsComplexTail = complexRules.ContainsKey
        member this.HeadsByComplexTail c = 
            if complexRules.ContainsKey c
            then complexRules.Item c
            else []