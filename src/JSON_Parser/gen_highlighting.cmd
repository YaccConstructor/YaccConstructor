del log.txt

echo JSON.yrd >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i JSON.yrd -c ExpandEbnf -c ExpandMeta -c Linearize -c "ReplaceLiterals KW_%%s" ^
	-g "RNGLRGenerator -pos array<Position<JetBrains.ReSharper.Psi.Tree.ILiteralExpression>> -module JSON.Parser -translate true -highlighting true -namespace JSONHighlighting -table LR -o JSONParser.fs" >> log.txt
