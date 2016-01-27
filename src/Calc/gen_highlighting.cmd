del log.txt

..\..\Bin\Release\v40\AbstractLexer.Generator.exe Lexer.fsl --unicode -o Lexer.fs --abstract

echo calc.yrd >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -f YardFrontend -i ..\..\src\YC.GrammarZOO\Calc\calc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -module Calc.AbstractParser -token FSA<char*Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> -translate false -highlighting true -namespace CalcHighlighting -pos uint64 -table LALR -abstract true -o Calc.yrd.fs " -D ca >> log.txt
