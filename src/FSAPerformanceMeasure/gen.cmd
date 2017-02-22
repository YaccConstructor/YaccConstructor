..\..\Bin\Release\v40\YC.YaccConstructor.exe -i "..\YC.GrammarZOO\Bio\16s\R16S_1_18.yrd" -c ExpandMeta ^
        -g "GLLGenerator -debug true -pos int -token int -module GLL.R16S_1_18  -o R16S_1_18.yrd.fs " >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i "..\YC.GrammarZOO\Bio\16s\R16S_1_18.yrd" -c ExpandMeta -c ExpandEbnf ^
        -g "GLLGenerator -debug true -token int -module GLL.R16S_1_18_noEBNF  -o R16S_1_18_noEBNF.yrd.fs " >> log.txt