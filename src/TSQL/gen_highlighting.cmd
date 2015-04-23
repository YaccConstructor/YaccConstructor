rem -c "ReplaceLiterals KW_%%s"
del log.txt

rem echo mssql.yrd >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -f YardFrontend -i mssql.yrd -c ExpandEbnf -c Linearize ^
        -g "RNGLRGenerator -module Yard.Examples.MSParser -translate false -namespace TSQLHighlighting -table LALR -o MSParser.fs -abstract true" >> log.txt