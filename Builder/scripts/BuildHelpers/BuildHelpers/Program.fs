module YaccConstructor.Build.Helpers

open System.Xml
open System.Xml.Linq
open System.Linq

let LoadProjFile (file:string) =
    XDocument.Load file    

let RemoveDependevces dependences (projFile:XDocument) =
    let nmspc = XNamespace.Get "http://schemas.microsoft.com/developer/msbuild/2003"
    let findIncudes name =
        projFile.Root.Descendants(nmspc + name)
         .Where(fun (t:XElement) -> List.exists (fun d -> t.Attribute(XName.Get "Include").Value = d) dependences)
    let refs = 
        seq { 
            yield! findIncudes "ProjectReference"
            yield! findIncudes "Reference"
            }
    refs.Remove()

let JustRmDependences dependences path =
    let proj = LoadProjFile path    
    RemoveDependevces dependences proj
    proj.Save path