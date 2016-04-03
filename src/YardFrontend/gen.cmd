..\..\Bin\Release\v40\YC.FsLex.exe --unicode Lexer.fsl
echo > log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -f FsYaccFrontend -i Parser.fsy ^
        -g "RNGLRGenerator -o Parser.fs -module Yard.Frontends.YardFrontend.GrammarParser -pos Source.Position -token Source.t" > log.txt