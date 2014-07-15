del log.txt

echo JSON.yrd >> log.txt
    ..\..\bin\Debug\v%1\YaccConstructor.exe -i JSON.yrd -c ExpandEbnf -c ExpandMeta -c Linearize -c "ReplaceLiterals KW_%%s" ^
	-g "RNGLRGenerator -pos array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> -token string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> -module JSON.Parser -translate true -highlighting true -namespace JSONHighlighting -table LR -o JSONParser.fs" >> log.txt
