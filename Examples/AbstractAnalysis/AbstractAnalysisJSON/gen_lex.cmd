..\..\..\YaccConstructor\AbstractLexer.Generator\bin\Release\AbstractLexer.Generator.exe Lexer.fsl --unicode -o Lexer.fs

..\..\..\YaccConstructor\YaccConstructor\bin\Release\YaccConstructor.exe -i JSON.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -o JSON.yrd.fs" >> log.txt