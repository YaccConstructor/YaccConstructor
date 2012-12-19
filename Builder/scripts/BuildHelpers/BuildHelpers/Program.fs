module YaccConstructor.Build.Helpers

open System.Xml
open System.Xml.Linq
open System.Linq

let private nmspc = XNamespace.Get "http://schemas.microsoft.com/developer/msbuild/2003"

let LoadProjFile (file:string) =
    XDocument.Load file    

let RemoveDependevces dependences (projFile:XDocument) =
    let refs = 
        projFile.Root.Descendants(nmspc + "Reference")
            .Where(fun (t:XElement) -> List.exists (fun d -> t.Attribute(XName.Get "Include").Value = d) dependences)
    refs.Remove()

do 
    let path =  @"D:\projects\YC\recursive-ascent\Builder\scripts\BuildHelpers\BuildHelpers\BuildHelpers1.fsproj"
    let proj = LoadProjFile path
    RemoveDependevces ["mscorlib"] proj
    proj.Save path