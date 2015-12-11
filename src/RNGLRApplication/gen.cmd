@echo off

del log.txt                                                                                      

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Calc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Calc -translate false -table LR -abstract false -o Calc.yrd.fs " >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleAmb.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.SimpleAmb -translate false -table LR -abstract false -o SimpleAmb.yrd.fs " >> log.txt
