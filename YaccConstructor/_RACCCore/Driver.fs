namespace Yard.Generators._RACCGenerator

type CoreDriver() =
    class
        let parse (lexer: ILexer<_,_>) lexbuf tables=
            //let tablesInterpretator = TableInterpreter()
            TableInterpreter.run lexer lexbuf tables
            

        let interprete inpForest = ()

        member self.Parse lexer lexbuf = parse lexer lexbuf
        member self.Interprete inpForest = interprete inpForest 
    end
