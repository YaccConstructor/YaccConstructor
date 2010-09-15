open  Yard.Generators._RACCGenerator

let fa = 
    {
        NIDToStateMap = dict [(1,1);(2,2);(3,3);(4,4);(5,5);(6,6)]
        NStartState   = 1
        NFinaleState  = 6
        NRules        = Set.ofList [{FromStateID = 1; Symbol = Epsilon; Label = "l1"; ToStateID = 2}
                                   ;{FromStateID = 2; Symbol = NSymbol("a"); Label = "l2"; ToStateID = 3}
                                   ;{FromStateID = 3; Symbol = Epsilon; Label = "l3"; ToStateID = 4}
                                   ;{FromStateID = 1; Symbol = Epsilon; Label = "l4"; ToStateID = 5}
                                   ;{FromStateID = 5; Symbol = NSymbol("b"); Label = "l5"; ToStateID = 6}
                                   ;{FromStateID = 6; Symbol = Epsilon; Label = "l6"; ToStateID = 4}
                                  ]    
    }

let fa2 = 
    {
        NIDToStateMap = dict [(1,1);(2,2);(3,3);(4,4)]
        NStartState  = 1
        NFinaleState = 4
        NRules        = Set.ofList [{FromStateID = 1; Symbol = Epsilon; Label = "l1"; ToStateID = 2}
                                   ;{FromStateID = 2; Symbol = NSymbol("a"); Label = "l"; ToStateID = 3}
                                   ;{FromStateID = 3; Symbol = Epsilon; Label = "l2"; ToStateID = 4}
                                   ;{FromStateID = 2; Symbol = Epsilon; Label = "l3"; ToStateID = 3}
                                   ;{FromStateID = 3; Symbol = Epsilon; Label = "l4"; ToStateID = 2}                                   
                                  ]    
    }

let fa3 = 
    {
        NIDToStateMap = dict [(1,1);(2,2);(3,3);(4,4);(5,5);(6,6)]
        NStartState    = 1
        NFinaleState = 6
        NRules        = Set.ofList [{FromStateID = 1; Symbol = Epsilon; Label = "SCls"; ToStateID = 2}
                                   ;{FromStateID = 2; Symbol = Epsilon; Label = "Sa"; ToStateID = 3}
                                   ;{FromStateID = 3; Symbol =  NSymbol("a"); Label = "l"; ToStateID = 4}
                                   ;{FromStateID = 4; Symbol = Epsilon; Label = "Ea"; ToStateID = 5}
                                   ;{FromStateID = 5; Symbol = Epsilon; Label = "ECls"; ToStateID = 6}
                                   ;{FromStateID = 2; Symbol = Epsilon; Label = "l"; ToStateID = 5}                                   
                                   ;{FromStateID = 5; Symbol = Epsilon; Label = "l"; ToStateID = 2}
                                  ]    
    }
do 
   printf "%A" (NLFAToDLFA.NLFAToDLFA fa3 (fun x -> List.filter ((<>)"l") x))
   |> ignore
   System.Console.ReadLine()
   |> ignore

