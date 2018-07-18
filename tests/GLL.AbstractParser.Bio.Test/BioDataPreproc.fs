module BioDataPreproc

open VDS.RDF
open VDS.RDF.Parsing

open System.IO
open System.Collections.Generic

let loadFromFile (file:string) =
    let g = new Graph()
    if (System.IO.Path.GetExtension file).ToLower() = "ttl"
    then        
        let ttlparser = new TurtleParser()
        ttlparser.Load(g, file)
    else
        FileLoader.Load(g, file)       
    g


let prots = new HashSet<string>([||])
let genes = new HashSet<string>([||])

let writeTriplesFromHomologene file (sw:StreamWriter) =
    let lines = File.ReadLines(file)
    
    for l in lines do
        let elems = l.Split('\t')
        let homoloGeneGroup = "HomoloGene_" + elems.[0] 
        let gene = "Gene_" + elems.[2]
        if genes.Contains(gene)
        then 
            sw.WriteLine(gene + "\t" + "is_homologous_to" + "\t" + homoloGeneGroup)
            sw.WriteLine(homoloGeneGroup + "\t" + "-is_homologous_to" + "\t" + gene)
    
let writeTriplesFromKegg mapFile keggFile (sw:StreamWriter) =
//    let keggLines = File.ReadLines(keggFile)
//    for l in keggLines do
//        let elems = l.Split('\t')
//        let pathw = "Pathway_" + elems.[0]
//        let namePath = elems.[1]
//        sw.WriteLine(pathw + "\t" + "name_pathway" + "\t" + namePath)  

    let mapLines = File.ReadLines(mapFile)
    for l in mapLines do
        let elems = l.Split('\t')
        let gene = "Gene_" + elems.[0]
        let pathw = "Pathway_" + elems.[1]
        if genes.Contains(gene)
        then 
            sw.WriteLine(gene + "\t" + "belongs_to" + "\t" + pathw)
            sw.WriteLine(pathw + "\t" + "-belongs_to" + "\t" + gene)

let writeTriplesFromSTRING mapFile stringFiles (sw:StreamWriter) =
    let mapLines = File.ReadLines(mapFile)
    let dict = new Dictionary<string, string>()
    for l in mapLines do
        let elems = l.Split('\t')
        if not(dict.ContainsKey elems.[1]) 
        then dict.Add(elems.[1], elems.[0])
    for f in stringFiles do
        if f = @"..\..\..\data\BioData\STRING\9606.protein.links.v10.txt"
        then
            let interacts = File.ReadLines(f)
            for i in interacts do
                let elems = i.Split(' ')
                let prot1 = dict.TryGetValue  elems.[0]
                let prot2 = dict.TryGetValue  elems.[1]
                match prot1, prot2 with
                | (true, p1),(true, p2) -> 
                    sw.WriteLine("Protein_" + p1 + "\t" + "interacts_with" + "\t" + "Protein_" + p2)
                    sw.WriteLine("Protein_" + p2 + "\t" + "interacts_with" + "\t" + "Protein_" + p1)
                | _ -> ()

let writeTriplesFromInterpro mapFile (file:string) (sw:StreamWriter) =
//    let settings = new System.Xml.XmlReaderSettings()
//    settings.DtdProcessing <- System.Xml.DtdProcessing.Parse
//    let reader = System.Xml.XmlReader.Create(file, settings)
//    while reader.Read()
//        do
//            if reader.Name.Equals "interpro" && reader.IsStartElement()
//            then 
//                let interproId = "FamilyOrDomain_" + reader.GetAttribute(0)
//                let proteinCount = reader.GetAttribute(1)
//                let shortName = reader.GetAttribute(2)
//                let interproType = reader.GetAttribute(3)
//                reader.Read() |> ignore
//                reader.Read() |> ignore
//                let name = reader.ReadString()
//                sw.WriteLine(interproId + "\t" + "protein_count" + "\t" + proteinCount)
//                sw.WriteLine(interproId + "\t" + "short_name" + "\t" + shortName)
//                sw.WriteLine(interproId + "\t" + "type" + "\t" + interproType)
//                sw.WriteLine(interproId + "\t" + "name" + "\t" + name)

    let mapLines = File.ReadLines(mapFile)
    for l in mapLines do
        let elems = l.Split('\t')
        let protein = "Protein_" + elems.[0]
        if prots.Contains(protein)
        then 
            let arrayInterPro = elems.[1].Split(';')
            for i = 0 to arrayInterPro.Length - 2 do
                let InterProId = "FamilyOrDomain_" + arrayInterPro.[i]
                sw.WriteLine(protein + "\t" + "has" + "\t" + InterProId)
                sw.WriteLine(InterProId + "\t" + "-has" + "\t" + protein)

let writeTriplesFromEntrezGene mapFile files (sw:StreamWriter) numberOfGenes =
//    for f in files do
//        if f = @"..\..\..\data\BioData\EntrezGene\Homo_sapiens.gene_info"
//        then
//            let lines = File.ReadAllLines(f)
//            for i = 1 to lines.Length - 1 do
//                let elems = lines.[i].Split('\t')
//
//                let tax_id = elems.[0]
//                let Gene = "Gene_" + elems.[1]
//                let Symbol = elems.[2]
//                let LocusTag = elems.[3]
//                let Synonyms = elems.[4]
//                let dbXrefs = elems.[5]
//                let chromosome = elems.[6]
//                let map_location = elems.[7]
//                let description = elems.[8]
//                let type_of_gene = elems.[9]
//                let Symbol_from_nomenclature_authority = elems.[10]
//                let Full_name_from_nomenclature_authority = elems.[11]
//                let Nomenclature_status = elems.[12]
//                let Other_designations = elems.[13]
//                let Modification_date = elems.[14]
//
//                sw.WriteLine(Gene + "\t" + "tax_id" + "\t" + tax_id)
//                sw.WriteLine(Gene + "\t" + "Symbol" + "\t" + Symbol)
//                sw.WriteLine(Gene + "\t" + "LocusTag" + "\t" + LocusTag)
//                sw.WriteLine(Gene + "\t" + "Synonyms" + "\t" + Synonyms)
//                sw.WriteLine(Gene + "\t" + "dbXrefs" + "\t" + dbXrefs)
//                sw.WriteLine(Gene + "\t" + "chromosome" + "\t" + chromosome)
//                sw.WriteLine(Gene + "\t" + "description" + "\t" + description)
//                sw.WriteLine(Gene + "\t" + "type_of_gene" + "\t" + type_of_gene)
//                sw.WriteLine(Gene + "\t" + "map_location" + "\t" + map_location)
//                sw.WriteLine(Gene + "\t" + "Symbol_from_nomenclature_authority" + "\t" + Symbol_from_nomenclature_authority)
//                sw.WriteLine(Gene + "\t" + "Full_name_from_nomenclature_authority" + "\t" + Full_name_from_nomenclature_authority)
//                sw.WriteLine(Gene + "\t" + "Nomenclature_status" + "\t" + Nomenclature_status)
//                sw.WriteLine(Gene + "\t" + "Other_designations" + "\t" + Other_designations)
//                sw.WriteLine(Gene + "\t" + "Modification_date" + "\t" + Modification_date)
    
    let mapLines = File.ReadAllLines(mapFile)
    let humanGenesFirst = 3887 // human genes are between 3887 and 24087 lines
    for x in humanGenesFirst..(humanGenesFirst + numberOfGenes) do
        let elems = mapLines.[x].Split('\t')
        let protein = "Protein_" + elems.[0]
        prots.Add(protein) |> ignore
        let arrayGene =elems.[1].Split(';')
        for i = 0 to arrayGene.Length - 2 do
            let gene = "Gene_" + arrayGene.[i]
            genes.Add(gene) |> ignore
            sw.WriteLine(protein + "\t" + "-codes_for" + "\t" + gene)
            sw.WriteLine(gene + "\t" + "codes_for" + "\t" + protein)

let writeTriplesFromGO mapFile file (sw:StreamWriter) =
//    let g = loadFromFile file

    let edg (f: VDS.RDF.INode) (t: VDS.RDF.INode) (l: VDS.RDF.INode) = 
        match f, t, l with
        | f, t, l when f.ToString().StartsWith "http://purl.obolibrary.org/obo" ->
            let GOid = (f :?> UriNode).Uri.Segments.[(f :?> UriNode).Uri.Segments.Length - 1]
            sw.WriteLine(GOid + "\t" + l.ToString() + "\t" + t.ToString())            
        | _ -> sw.WriteLine(f.ToString() + "\t" + l.ToString() + "\t" + t.ToString())  
            
//    for t in g.Triples do
//        edg t.Object t.Subject t.Predicate

    let mapLines = File.ReadLines(mapFile)
    for l in mapLines do
        let elems = l.Split('\t')
        let protein = "Protein_" + elems.[0]
        if prots.Contains(protein)
        then 
            let arrayGO =elems.[1].Split(';')
            for g in arrayGO do
                let go = "GO_" + g.Substring(g.IndexOf(':') + 1)
                sw.WriteLine(protein + "\t" + "has" + "\t" + go)
                sw.WriteLine(go + "\t" + "-has" + "\t" + protein)

let writeAllTriples basePath (sw:StreamWriter) numberOfGenes =

    writeTriplesFromEntrezGene (basePath + "\map\UniprotToEntrezGene.txt") (System.IO.Directory.GetFiles(basePath + "\EntrezGene")) sw numberOfGenes
    writeTriplesFromKegg (basePath + "\map\geneToPath.txt") (basePath + "\KEGG\pathways.keg") sw
//    writeTriplesFromSTRING (basePath + "\map\UniprotToString.txt") (System.IO.Directory.GetFiles (basePath + "\STRING")) sw
    writeTriplesFromInterpro (basePath + "\map\UniprotToInterpro.txt") (basePath + "\InterPro\interpro.xml") sw
    writeTriplesFromGO (basePath + "\map\UniprotToGO.txt") (basePath + "\GeneOntology\go.owl") sw
    writeTriplesFromHomologene (basePath + "\HomoloGene\homologene.data.txt") sw
    sw.Close()
    printfn "file with %i genes created" numberOfGenes  
    
let outputDir = __SOURCE_DIRECTORY__ + @"\..\data\BioData\result\"
let dataDir = (__SOURCE_DIRECTORY__ + @"\..\data\BioData\")

let preprocBioData() = 
    let basePath = dataDir
    let numberOfGenes = [|20; 50; 100; 150; 200; 250; 300; 350; 400|]

    for n in numberOfGenes do
        let filePath = outputDir + (sprintf "%igenes_AllDatabases.txt" n)
        System.IO.Directory.CreateDirectory (System.IO.Path.GetDirectoryName filePath)
        let sw = new StreamWriter(filePath)
        sw.AutoFlush <- true
        writeAllTriples basePath sw n