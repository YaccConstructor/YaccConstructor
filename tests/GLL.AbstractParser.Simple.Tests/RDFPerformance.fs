module YC.GLL.Abstarct.Tests.RDFPerformance

open VDS.RDF
open VDS.RDF.Parsing

let loadFromFile (file:string) =
    let g = new Graph()
    if (System.IO.Path.GetExtension file).ToLower() = "ttl"
    then        
        let ttlparser = new TurtleParser()
        ttlparser.Load(g, file)
    else
        FileLoader.Load(g, file)        
        
    g
