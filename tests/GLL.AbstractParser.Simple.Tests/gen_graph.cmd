..\..\Bin\Release\v40\YC.YaccConstructor.exe -i StrangeBrackets.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.StrangeBrackets -translate false -o StrangeBrackets.yrd.fs -abstract true" >> log.txt
