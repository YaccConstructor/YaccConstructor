..\..\Bin\Release\v40\AbstractLexer.Generator.exe --unicode Lexer.fsl
..\..\Bin\Release\v40\YC.YaccConstructor.exe -f FsYaccFrontend -i Parser.fsy ^
    -g "RNGLRGenerator -o Parser.fs -module Yard.Frontends.YardFrontend.GrammarParser -pos Source.Position -token Source.t"