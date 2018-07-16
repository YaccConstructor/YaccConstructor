..\..\tools\fslexyacc\fslex --unicode --lexlib Microsoft.FSharp.Text.Lexing fsyacclex.fsl
..\..\tools\fslexyacc\fsyacc --internal --module FSharp.PowerPack.FsYacc.Parser --lexlib Microsoft.FSharp.Text.Lexing  --parslib Microsoft.FSharp.Text.Parsing fsyaccpars.fsy
