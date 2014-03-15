del log.txt

echo calc.yrd >> log.txt
    ..\..\bin\Release\v%1\YaccConstructor.exe -i calc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos uint64 -token string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> -module Calc.AbstractParser -translate true -highlighting true -namespace CalcHighlighting -table LR -o Calc.yrd.fs" >> log.txt
