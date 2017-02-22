..\..\Bin\Release\v40\YC.YaccConstructor.exe -i StrangeBrackets.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.StrangeBrackets -o StrangeBrackets.yrd.fs -abstract true"

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Brackets.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.Brackets -o Brackets.yrd.fs -abstract true" >> log.txt


..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Brackets2.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.Brackets2 -o Brackets2.yrd.fs -abstract true" >> log.txt


..\..\Bin\Release\v40\YC.YaccConstructor.exe -i GPPerf1.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.GPPerf1 -o GPPerf1.yrd.fs -abstract true" >> log.txt


..\..\Bin\Release\v40\YC.YaccConstructor.exe -i GPPerf2.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.GPPerf2 -o GPPerf2.yrd.fs -abstract true" >> log.txt
