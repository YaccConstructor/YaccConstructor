del log.txt

..\..\Bin\Release\v40\YC.AbstractLexer.Generator.exe Lexer.fsl --unicode -o Lexer.fs

echo calc.yrd >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -f YardFrontend -i extCalc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos uint64 -module ExtCalc.AbstractParser -highlighting true -namespace ExtCalcHighlighting -table LALR -abstract true -o ExtCalc.yrd.fs" >> log.txt
