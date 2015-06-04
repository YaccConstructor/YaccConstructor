rem -c "ReplaceLiterals KW_%%s"
del log.txt

..\..\Bin\Release\v40\AbstractLexer.Generator.exe ..\YC.Grammar\SQL\TSQL\LexerAbstract.fsl -o Lexer.fs --unicode  --abstract

rem echo mssql.yrd >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -f YardFrontend -i ..\YC.Grammar\SQL\TSQL\mssql_abstract.yrd -c ExpandEbnf -c Linearize ^
        -g "RNGLRGenerator -module Yard.Examples.MSParser -translate true -highlighting true -namespace TSQLHighlighting -table LALR -o MSParser.fs -abstract true" >> log.txt