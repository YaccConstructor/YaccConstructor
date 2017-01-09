module YC.GLL.Abstarct.Tests.RDFPerformance

open VDS.RDF
open VDS.RDF.Parsing

let loadFromTTL (file:string) =
    let g = new Graph()
    let ttlparser = new TurtleParser()
    ttlparser.Load(g, file)
    g
