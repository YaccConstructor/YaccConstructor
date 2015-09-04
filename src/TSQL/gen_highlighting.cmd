rem -c "ReplaceLiterals KW_%%s"
del log.txt

..\..\Bin\Release\v40\AbstractLexer.Generator.exe ..\YC.GrammarZOO\SQL\TSQL\LexerAbstract.fsl -o Lexer.fs --unicode  --abstract

rem echo mssql.yrd >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -f YardFrontend -i ..\YC.GrammarZOO\SQL\TSQL\mssql_abstract.yrd -c ExpandEbnf -c Linearize ^
        -g "RNGLRGenerator -module Yard.Examples.MSParser -translate false -highlighting true -namespace TSQLHighlighting -table LALR -o MSParser.fs -abstract true" >> log.txt