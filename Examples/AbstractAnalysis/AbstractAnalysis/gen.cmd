..\..\..\YaccConstructor\AbstractLexer.Generator\bin\Release\AbstractLexer.Generator.exe Lexer.fsl --unicode -o Lexer.fs

..\..\..\YaccConstructor\YaccConstructor\bin\Release\YaccConstructor.exe -i Calc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -o Calc.yrd.fs" >> log.txt
