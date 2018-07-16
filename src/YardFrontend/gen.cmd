..\..\tools\fslexyacc\fslex --unicode Lexer.fsl
cd ../YC.FsYacc
dotnet run "../YardFrontend/parser.fsy" "-o ../YardFrontend/Parser.fs -module Yard.Frontends.YardFrontend.GrammarParser -pos SourcePosition -token Source
echo > log.txt