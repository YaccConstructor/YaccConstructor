// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.IO
open System.Drawing

open Drawing
open BioParser
open DataPreprocessing

let grammar = @"../../../../src/YC.GrammarZOO/Bio/tests/bio_brackets.yrd"
let parser = new BioParser(grammar)

let drawPositiveExamples isGpu legend fastaFile =
    let data = getDataFrom16sBase fastaFile
    let path = "../../positive/"
    Directory.CreateDirectory(path) |> ignore
    for (id, gen) in data do        
        let picture = new ParsingPicture (gen.Length, parser.Parse isGpu gen)
        picture.Draw(legend, path + id + ".bmp")

let drawNegativeExamples isGpu minLength maxLength legend fastaFiles =
    let path = "../../negative/"
    let random = System.Random()
    Directory.CreateDirectory(path) |> ignore
    for f in fastaFiles do
        let id, gen, intervals16s = getCompleteGenomeData f
        let filteredGen = removeIntervals gen intervals16s
        for i in 0 .. 300 .. filteredGen.Length - maxLength - 1 do
            let length = random.Next(minLength, maxLength)
            let name = sprintf "%s_%i_%i.bmp" id i length
            let picture = new ParsingPicture(length, parser.Parse isGpu filteredGen.[i .. i + length - 2])
            picture.Draw(legend, (path + name))

[<EntryPoint>]
let main argv = 
    let genomeFiles = 
        Directory.GetFiles("../../../data/bio/complete_genome/", "*.txt", SearchOption.AllDirectories)
    let legend = [(parser.StartNonTerm, Color.Black)]
    drawPositiveExamples false legend  "../../SILVA_128_SSURef_Nr99_tax_silva_first_500k_lines.fasta"
    drawNegativeExamples true 1300 1700 legend genomeFiles
    0