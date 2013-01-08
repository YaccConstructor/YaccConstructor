#I "fake"
#r "FakeLib.dll"

open Fake

open System.IO

let workDir = @".\workdir\"
let srcDir = Path.Combine(workDir, "src")
let distrDir = Path.Combine(workDir, "distr")
let zipDir = Path.Combine(workDir, "zip")
let distrYCDir = Path.Combine(distrDir, "YC")
let solutionRootDir = Path.Combine(srcDir, "YaccConstructor")
let productName = "YaccConstructor"
let version = ref "0.0.0.0"
let prdNameSuffix = ref ""

// Targets
Target "Clean" (fun _ -> 
    CleanDirs [distrDir;distrYCDir;zipDir]
)

Target "CollectAllBinaries" (fun _ ->
    !+ (solutionRootDir + "/**/bin/Release/*.exe")
        ++ (solutionRootDir + "/**/bin/Release/*.dll")
        |> Scan
        |> CopyTo distrYCDir
)

Target "ZipAll" (fun _ ->
    !! (distrDir + "/**/*")
    |> Zip distrDir (Path.Combine(zipDir, productName + "_" + !version + ".zip"))
)


"Clean" ==> "CollectAllBinaries" ==> "ZipAll"

version := getBuildParam "ver"

// Run
Run "ZipAll"