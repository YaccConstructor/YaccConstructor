module ComplexTest

open Yard.Core
open ConversionsTests

let path = @"C:\gsv\projects\YC\YaccConstructor\src\YC.GrammarZOO\Bio\16s\R16S_19_27.yrd"

let _do() =    
    let loadIL = fe.ParseGrammar path
    Namer.initNamer loadIL.grammar
    let result = 
        let il = 
            loadIL 
            |> applyConversion expandEbnf        
            |> applyConversion expandMeta        
            |> applyConversion expandRepeat
            |> applyConversion expandInnerAlt        
            |> applyConversion expandBrackets
            |> applyConversion expandTopLevelAlt
            //|> applyConversion expandBrackets

        il        
        |> applyConversion expandInnerAlt        
        |> applyConversion expandBrackets
        |> applyConversion expandTopLevelAlt        

    result.grammar.[0].rules
    |> List.map (fun r -> r.name.text + ":" + r.body.ToString())
    |> String.concat "\n"
    |> fun text -> System.IO.File.WriteAllText("outGrammar.txt",text.ToString())

_do()