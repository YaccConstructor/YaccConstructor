#I "D:/soft/FAKE/"
#r "FakeLib.dll"

open Fake

open System.Diagnostics
open System.IO

let shellExecute program args =
    let startInfo = new ProcessStartInfo()
    startInfo.FileName <- program
    startInfo.Arguments <- args    

    let proc = Process.Start(startInfo)
    proc.WaitForExit()
    ()


let buildDir = @".\bin\"

#load "solution.fsx"
#load "project.fsx"

Target "GetCommonSrc"
    (fun _ ->
        Directory.GetFiles(Solution.Common.SvnSrcFolder,"*.fs")
        |> CopyTo Solution.Common.Folder
    )

Target "BuildCommon" 
   (fun _ -> 
       MSBuildRelease buildDir "Build" [Solution.Common.Path]
       |> Log "AppBuild-Output: ")

Target "CommonSendLib"
    ( fun _ ->
        [@"./bin/Common.dll"] 
        |> fun x -> 
            CopyTo Solution.Convertions.LibPath x
            x        
        |> fun x -> 
            CopyTo Solution.Main.LibPath x
            x
        |> CopyTo Solution.Yard.LibPath
    )

Target "GetConvertionsSrc"
    (fun _ ->
        Directory.GetFiles(Solution.Convertions.SvnSrcFolder,"*.fs")
        |> CopyTo Solution.Convertions.Folder
    )

Target "BuildConvertions"
    (fun _ ->
       MSBuildRelease buildDir "Build" [Solution.Convertions.Path]
       |> Log "AppBuild-Output: "    
    )               

Target "ConvertionsSendLib"
    ( fun _ ->        
        [@"./bin/Convertions.dll"] 
        |> CopyTo Solution.Main.LibPath
    )

Target "CopyIrony"
    ( fun _ ->        
        [@"./3rdParty/Irony.dll"] 
        |> CopyTo Solution.Main.LibPath
    )

Target "GetMainSrc"
    (fun _ ->
        Directory.GetFiles(Solution.Main.SvnSrcFolder,"*.fs")
        |> CopyTo Solution.Main.Folder
    )

Target "BuildMain"
    (fun _ ->
       MSBuildRelease buildDir "Build" [Solution.Main.Path]
       |> Log "AppBuild-Output: "    
    )               

Target "GetYardSrc"
    (fun _ ->
        Directory.GetFiles(Solution.Yard.SvnSrcFolder,"*.fs")
        |> List.ofSeq
        |> fun x -> 
               Directory.GetFiles(Solution.Yard.SvnSrcFolder,"*.fsl")
               |> List.ofSeq
               |> fun y -> y @ x
        |> fun x -> 
               Directory.GetFiles(Solution.Yard.SvnSrcFolder,"*.fsy")
               |> List.ofSeq
               |> fun y -> y @ x
        |> fun x -> 
               Directory.GetFiles(Solution.Yard.SvnSrcFolder,"*.fsi")
               |> List.ofSeq
               |> fun y -> y @ x
        |> CopyTo Solution.Yard.Folder
    )

Target "BuildYard"
    (fun _ ->
       MSBuildRelease buildDir "Build" [Solution.Yard.Path]
       |> Log "AppBuild-Output: "    
    )               


Target "Clean" 
    (fun _ -> 
        [buildDir; Solution.Main.LibPath; Solution.Convertions.LibPath; Solution.Yard.LibPath]
        |> CleanDirs
    )

Target "BuildAll" (fun _ -> ())

// Dependencies
"BuildCommon" <== ["GetCommonSrc"; "Clean"]
"CommonSendLib" <== ["BuildCommon"]
"BuildConvertions" <== ["GetConvertionsSrc"; "CommonSendLib"]
"ConvertionsSendLib" <== ["BuildConvertions"]
"BuildMain" <== ["GetMainSrc"; "ConvertionsSendLib"; "CommonSendLib"]

"BuildYard" <== ["BuildCommon"; "GetYardSrc"]

"BuildAll" <== ["BuildMain"; "BuildYard"]

// Run
//Run "BuildAll"
printfn "param:  %A" (getBuildParam "mode")
