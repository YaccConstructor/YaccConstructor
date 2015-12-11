del log.txt

..\..\Bin\Release\v40\AbstractLexer.Generator.exe Lexer.fsl --unicode -o Lexer.fs --abstract

echo calc.yrd >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -f YardFrontend -i extCalc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos uint64 -module ExtCalc.AbstractParser -translate false -highlighting true -namespace ExtCalcHighlighting -table LALR -abstract true -o ExtCalc.yrd.fs" >> log.txt
