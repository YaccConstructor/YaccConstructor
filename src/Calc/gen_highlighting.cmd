del log.txt

echo calc.yrd >> log.txt
    ..\packages\YaccConstructor.0.0.8.12\tools\YaccConstructor\YC.YaccConstructor.exe -f YardFrontend -i calc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos uint64 -token string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> -module Calc.AbstractParser -translate true -highlighting true -namespace CalcHighlighting -table LALR -o Calc.yrd.fs" >> log.txt
