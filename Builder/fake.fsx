#I "e:/fake/FAKE/"
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
// load "project.fsx"

Target "GetCommonSrc"
    (fun _ ->
        Directory.GetFiles(Solution.Common.SvnSrcFolder,"*.fs")
        |> CopyTo Solution.Common.Folder
    )

Target "BuildCommon" 
   (fun _ -> 
       MSBuild buildDir "Build" Solution.Common.BuildProperties [Solution.Common.Path]
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
        |> fun x -> 
            CopyTo Solution.RACCGenerator.LibPath x
            x
        |> CopyTo Solution.Yard.LibPath
    )

Target "CreateLibFolder" 
    (fun _ -> List.iter (fun (p:Projects.Project) -> CreateDir p.LibPath) (!Solution.projects))

Target "GetConvertionsSrc"
    (fun _ ->
        Directory.GetFiles(Solution.Convertions.SvnSrcFolder,"*.fs")
        |> CopyTo Solution.Convertions.Folder
    )

Target "BuildConvertions"
    (fun _ ->
       MSBuild buildDir "Build" Solution.Convertions.BuildProperties [Solution.Convertions.Path]
       |> Log "AppBuild-Output: "    
    )               

Target "ConvertionsSendLib"
    ( fun _ ->        
        [@"./bin/Convertions.dll"] 
        |> fun x -> 
            CopyTo Solution.RACCGenerator.LibPath x
            x
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
       MSBuild buildDir "Build" Solution.Main.BuildProperties [Solution.Main.Path]
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
       MSBuild buildDir "Build" Solution.Yard.BuildProperties [Solution.Yard.Path]
       |> Log "AppBuild-Output: "    
    )               


Target "GetRACCSrc"
    (fun _ ->
        Directory.GetFiles(Solution.RACCCommon.SvnSrcFolder,"*.fs")
        |> CopyTo Solution.RACCCommon.Folder

        Directory.GetFiles(Solution.RACCFA.SvnSrcFolder,"*.fs")
        |> CopyTo Solution.RACCFA.Folder

        Directory.GetFiles(Solution.RACCCore.SvnSrcFolder,"*.fs")
        |> CopyTo Solution.RACCCore.Folder

        Directory.GetFiles(Solution.RACCGenerator.SvnSrcFolder,"*.fs")
        |> CopyTo Solution.RACCGenerator.Folder
    )

Target "BuildRACCCommon"
    (fun _ ->
       MSBuild buildDir "Build" Solution.RACCCommon.BuildProperties [Solution.RACCCommon.Path]
       |> Log "AppBuild-Output: "    
    )               

Target "BuildRACCCore"
    (fun _ ->
       MSBuild buildDir "Build" Solution.RACCCore.BuildProperties [Solution.RACCCore.Path]
       |> Log "AppBuild-Output: "    
    )               

Target "BuildRACCFA"
    (fun _ ->
       MSBuild buildDir "Build" Solution.RACCFA.BuildProperties [Solution.RACCFA.Path]
       |> Log "AppBuild-Output: "    
    )               

Target "BuildRACCGenerator"
    (fun _ ->
       MSBuild buildDir "Build" Solution.RACCGenerator.BuildProperties [Solution.RACCGenerator.Path]
       |> Log "AppBuild-Output: "    
    )               

Target "RACCCommonSendLib"
    ( fun _ ->        
        [@"./bin/RACCCommon.dll"] 
        |> fun x -> 
            CopyTo Solution.RACCGenerator.LibPath x
            x
        |> fun x -> 
            CopyTo Solution.RACCCore.LibPath x
            x
        |> CopyTo Solution.RACCFA.LibPath
    )

Target "RACCFASendLib"
    ( fun _ ->        
        [@"./bin/RACCFiniteAutomata.dll"] 
        |> fun x -> 
            CopyTo Solution.RACCGenerator.LibPath x
            x
        |> CopyTo Solution.RACCCore.LibPath
    )


Target "Clean" 
    (fun _ -> 
        [buildDir; Solution.Main.LibPath; Solution.Convertions.LibPath; Solution.Yard.LibPath]
        |> CleanDirs
    )

Target "BuildAll" (fun _ -> ())

Target "BuildRACC" (fun _ -> ())

// Dependencies
"BuildCommon" <== ["GetCommonSrc"; "Clean"; "CreateLibFolder"]
"CommonSendLib" <== ["BuildCommon"]
"BuildConvertions" <== ["GetConvertionsSrc"; "CommonSendLib"]
"ConvertionsSendLib" <== ["BuildConvertions"]
"BuildMain" <== ["GetMainSrc"; "ConvertionsSendLib"; "CommonSendLib"]

"RACCFASendLib" <== ["BuildRACCFA"]
"RACCCommonSendLib" <== ["BuildRACCCommon"]
"BuildRACCGenerator" <== ["CommonSendLib";"RACCFASendLib";"RACCCommonSendLib"; "ConvertionsSendLib"]
"BuildRACCFA" <== ["RACCCommonSendLib"]
"BuildRACCCore" <== ["RACCFASendLib";"RACCCommonSendLib"]
"BuildRACCCommon" <== ["GetRACCSrc"; "CreateLibFolder"]
"BuildRACC" <== ["BuildRACCFA";"BuildRACCCommon";"BuildRACCCore";"BuildRACCGenerator"]


"BuildYard" <== ["BuildCommon"; "GetYardSrc"]

"BuildAll" <== ["BuildMain"; "BuildYard"]

// Run
//Run "BuildAll"
Solution.SetNetVer (if getBuildParam "net" = "" then "4.0" else getBuildParam "net")
//Solution.SetMode 
  //  (let mode =  getBuildParam "mode"
    // if mode = "" 
   //  then Projects.Mode.Release 
    // else 
     //   if mode.ToLower() = "debug"
     //   then Projects.Mode.Debug
     //   else Projects.Mode.Release
     //)

Run (getBuildParam "target")