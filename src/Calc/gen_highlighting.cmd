del log.txt

..\..\Bin\Release\v40\AbstractLexer.Generator.exe Lexer.fsl --unicode -o Lexer.fs --abstract

echo calc_c.yrd >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -f YardFrontend -i ..\..\src\YC.GrammarZOO\Calc\calc_c.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos uint64 -module Calc.AbstractParser -translate false -highlighting true -namespace CalcHighlighting -table LALR -abstract true -o Calc.yrd.fs" >> log.txt
