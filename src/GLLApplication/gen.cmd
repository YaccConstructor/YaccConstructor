..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleAmb.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -abstract false -module GLL.SimpleAmb -o SimpleAmb.yrd.fs " >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i BadLeftRec.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -abstract false -module GLL.BadLeftRec -o BadLeftRec.yrd.fs " >> log.txt


..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Calc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -abstract false -module GLL.Calc -o Calc.yrd.fs " >> log.txt

