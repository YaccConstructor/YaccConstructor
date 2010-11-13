namespace Yard.Generators._RACCGenerator

type CoreDriver() =
    class
        let parse (lexer: ILexer<_>) tables=            
            TableInterpreter.run lexer tables
            

        let interprete inpForest = ()

        member self.Parse lexer = parse lexer
        member self.Interprete inpForest = interprete inpForest 
    end
