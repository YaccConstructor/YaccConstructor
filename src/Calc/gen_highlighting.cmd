del log.txt

..\..\Bin\Release\v40\YC.AbstractLexer.Generator.exe Lexer.fsl --unicode -o Lexer.fs

echo calc.yrd >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -f YardFrontend -i calc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos uint64 -token string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> -module Calc.AbstractParser -translate true -highlighting true -namespace CalcHighlighting -table LALR -o Calc.yrd.fs -abstract true" >> log.txt
