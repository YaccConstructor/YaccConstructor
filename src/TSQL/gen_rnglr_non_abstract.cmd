del log.txt

..\..\Bin\Release\v40\AbstractLexer.Generator.exe ..\YC.GrammarZOO\SQL\TSQL\Lexer.fsl -o Lexer.fs --unicode

..\..\Bin\Release\v40\YC.YaccConstructor.exe -f YardFrontend -i ..\YC.GrammarZOO\SQL\TSQL\mssql.yrd -c ExpandEbnf -c Linearize ^
	-g "RNGLRGenerator -module Yard.Examples.MSParser -translate false -table LALR -o MSParser.fs" >> log.txt