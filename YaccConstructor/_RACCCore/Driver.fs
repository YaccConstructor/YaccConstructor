namespace Yard.Generators._RACCGenerator

type CoreDriver(tables) =
    class
        let parse (lexer: ILexer<_,_>) lexbuf =
            let tablesInterpretator = TableInterpreter(tables)
            tablesInterpretator.Parse lexer lexbuf
            

        let interprete inpForest = ()

        member self.Parse lexer lexbuf = parse lexer lexbuf
        member self.Interprete inpForest = interprete inpForest 
    end
