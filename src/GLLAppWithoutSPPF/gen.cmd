
..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleAmb.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -withoutTree true -module GLL.SimpleAmb -o SimpleAmb.yrd.fs " >> log.txt
