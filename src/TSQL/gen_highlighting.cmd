del log.txt

rem echo mssql.yrd >> log.txt
    ..\..\bin\Debug\v%1\YaccConstructor.exe -f YardFrontend -i mssql.yrd -c ExpandEbnf -c Linearize -c "ReplaceLiterals KW_%%s"^
        -g "RNGLRGenerator -pos array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> -token string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> -module Yard.Examples.MSParser -translate true -highlighting true -namespace TSQLHighlighting -table LALR -o MSParser.fs" >> log.txt