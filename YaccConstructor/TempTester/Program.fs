open  Yard.Generators.RACC

let fa = 
    {
        IDToStateMap = dict [(1,1);(2,2);(3,3);(4,4);(5,5);(6,6)]
        StartStates  = Set.ofList [1]
        FinaleStates = Set.ofList [6]
        Rules        = Set.ofList [{FromStateID = 1; Symbol = Epsilon; Label = 'l'; ToStateID = 2}
                                   ;{FromStateID = 2; Symbol = NSymbol("a"); Label = 'l'; ToStateID = 3}
                                   ;{FromStateID = 3; Symbol = Epsilon; Label = 'l'; ToStateID = 4}
                                   ;{FromStateID = 1; Symbol = Epsilon; Label = 'l'; ToStateID = 5}
                                   ;{FromStateID = 5; Symbol = NSymbol("b"); Label = 'l'; ToStateID = 6}
                                   ;{FromStateID = 6; Symbol = Epsilon; Label = 'l'; ToStateID = 4}
                                  ]    
    }

let fa2 = 
    {
        IDToStateMap = dict [(1,1);(2,2);(3,3);(4,4)]
        StartStates  = Set.ofList [1]
        FinaleStates = Set.ofList [4]
        Rules        = Set.ofList [{FromStateID = 1; Symbol = Epsilon; Label = 'l'; ToStateID = 2}
                                   ;{FromStateID = 2; Symbol = NSymbol("a"); Label = 'l'; ToStateID = 3}
                                   ;{FromStateID = 3; Symbol = Epsilon; Label = 'l'; ToStateID = 4}
                                   ;{FromStateID = 2; Symbol = Epsilon; Label = 'l'; ToStateID = 3}
                                   ;{FromStateID = 3; Symbol = Epsilon; Label = 'l'; ToStateID = 2}                                   
                                  ]    
    }

do 
   printf "%A" (NLFAToDLFA.NLFAToDLFA fa)
   System.Console.ReadLine()

