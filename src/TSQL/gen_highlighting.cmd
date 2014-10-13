rem -c "ReplaceLiterals KW_%%s"
del log.txt

rem echo mssql.yrd >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -f YardFrontend -i mssql.yrd -c ExpandEbnf -c Linearize ^
        -g "RNGLRGenerator -pos array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> -module Yard.Examples.MSParser -translate true -highlighting true -namespace TSQLHighlighting -table LALR -o MSParser.fs" >> log.txt