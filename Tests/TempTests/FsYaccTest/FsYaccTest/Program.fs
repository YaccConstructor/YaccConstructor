open Microsoft.FSharp.Text.Lexing

open Yard.Generators.RACCGenerator

//Main.exe -i ../../RaccParser.yrd

type MyLexeme (tag,_value) =
    member self.MValue = _value
    interface ILexeme with    
       member self.tag = tag
       member self.CompareTo x =  compare (hash self) (hash x)
    end    


//type RaccLexer(lb) = 
//    let locBuf = ref []
//    interface ILexer<MyLexeme> with    
//        member self.Get pos = 
//            let l = !locBuf |> List.length
//            if l >= pos then 
//                (!locBuf).[l - pos]
//            else
//                let t = MyLexeme(1, Lexer.main lb) :> ILexeme
//                locBuf := t :: !locBuf
//                t
//    end

let run path =
    //Create lexer
    let content = System.IO.File.ReadAllText(path)
    let reader = new System.IO.StringReader(content)    
    let lexbuf = LexBuffer<_>.FromTextReader reader
    let l = RaccLexer.Lexer(lexbuf)

    //Create tables
    let tables = Yard.Generators.RACCGenerator.Tables.tables
        
    
    //Run parser
    // trees -- dirivation forest
    // cache -- trace cache
    // cc -- some additional debug info
    let parseRes,cache,cc = 
        let ti = new TableInterpreter()
        ti.Run l tables

    let result = 
        match parseRes with
        //Parse success
        | PSuccess (forest) -> 
        //run forest interpretation (action code calculation)
            printf "\nForest %A\n\n\n\n\n\nResult::\n" forest
            Seq.map 
             (fun tree -> ASTInterpretator.interp RACC.Actions.ruleToAction cache tree)
             forest
        //Error handling
        | PError (pos) -> 
            //Error handling
            //If you create lexeme with position in stream, you can not only provide error lexeme
            // but also navigate in error position
            let errLexeme = (l :> ILexer).Get(pos)
            "Incorrect input. Unexpected lexeme: " + string errLexeme.tag + " with value = " + errLexeme.ToString()
            |> failwith
            
    printf "\nResult %A\n" result
    

let () =
    printfn "Start.."
    let content = System.IO.File.ReadAllText("../../test.in")
    let reader = new System.IO.StringReader(content)
    let lexbuf = LexBuffer<_>.FromTextReader reader
//    let lexems = seq { 
//                       while not lexbuf.IsPastEndOfStream do
//                             yield Lexer.main lexbuf  
//                      }
//    lexems |> Seq.iter (fun x -> printf "%A\n" x)
//lexbuf.Lexeme
//    LexBuffer.LexemeString(lexbuf
    let res2 = run "../../test.in"
    printfn "%A" res2
    printfn "\n\n\nFSYACC:\n"
    let res = Parser.start Lexer.main lexbuf
    printfn "%A" res
//    List.fold
    ()