del log.txt

echo calc.yrd >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -f YardFrontend -i calc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos uint64 -module Calc.AbstractParser -translate true -highlighting true -namespace CalcHighlighting -table LALR -o Calc.yrd.fs" >> log.txt
