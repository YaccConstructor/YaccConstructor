// Learn more about F# at http://fsharp.net
(* 
1) Генератор пустого файла *.тех
2)обойти рулы и напечатать правила отдельно*)
//pdflatex latexpdf
//YaccConstructor.exe  -f YardFrontend -g TreeDump -i F:\CODING\SciProjects\1\RIP\Tests\Basic\test_alt\test_alt.yrd 

namespace Yard.Generators.TeXGenerator
open Yard.Core

type TeXGenerator() =
    inherit Generator()
        override this.Name = "TeXGenerator"
        override this.Generate t =
            let text = "\\documentclass{article}\n\\usepackage[cp1251]{inputenc}\n\n\\begin{document}\n" + t.grammar.ToString() + "\n\\end{document}" 
            System.IO.File.WriteAllText(t.info.fileName + ".tex", text) :> obj
        override this.AcceptableProductionTypes = []