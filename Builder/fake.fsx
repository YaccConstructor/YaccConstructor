#I "fake"
#r "FakeLib.dll"

open Fake

open System.IO

let workDir = @".\workdir\"
let rootDir = @".\workdir\..\..\"
let srcDir = Path.Combine(workDir, "src")
let distrDir = Path.Combine(workDir, "distr")
let zipDir = Path.Combine(workDir, "zip")
let distrYCDir = Path.Combine(distrDir, "YC")
let distrExamplesDir = Path.Combine(distrDir, "Examples")
let distrGTemplates = Path.Combine(distrDir, "GrammarTemplates")
let solutionRootDir = Path.Combine(srcDir)
let productName = "YaccConstructor"
let version = ref "0.0.0.0"
let prdNameSuffix = ref ""

let combine x y = Path.Combine(x,y)

let customCopy dest files = 
    files |> List.iter (fun f -> System.IO.File.Copy(f,(Path.GetFileName f) |> combine dest))

let getDistrName () = productName + "_" + !version

let rec DirectoryCopy sourceDirName destDirName copySubDirs =
        let dir = new DirectoryInfo(sourceDirName)
        let dirs = dir.GetDirectories()

        if not dir.Exists
        then
            failwith (
                "Source directory does not exist or could not be found: "
                + sourceDirName)

        if not (Directory.Exists destDirName)
        then  Directory.CreateDirectory(destDirName) |> ignore

        let files = dir.GetFiles()
        
        files 
        |> Seq.iter(fun file ->
            let temppath = Path.Combine(destDirName, file.Name)
            file.CopyTo(temppath, false) |> ignore)

        if copySubDirs
        then
            dirs
            |> Seq.iter(fun subdir ->
                let temppath = Path.Combine(destDirName, subdir.Name)
                DirectoryCopy subdir.FullName temppath copySubDirs)

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

Target "CollectLicensesStuff" (fun _ ->
    [ combine rootDir "COPYING"
     ;combine rootDir "README"]
    |> customCopy distrDir
)

Target "CollectExaples" (fun _ ->
    DirectoryCopy (combine workDir "Examples/Calc")  distrExamplesDir true
)

Target "CollectRedistGrammars" (fun _ ->
    DirectoryCopy (combine workDir "GrammarTemplates") distrGTemplates true
)

Target "GenVersionFile" (fun _ ->
	System.IO.File.WriteAllText(distrDir+"version", getDistrName())
)

Target "ZipAll" (fun _ ->
    !+ (distrDir + "/**/*")
    ++ (distrDir + "/*")
    |> Scan
    |> Zip distrDir (Path.Combine(zipDir, getDistrName + ".zip"))
)


"Clean" 
==> "CollectAllBinaries" 
==> "CollectLicensesStuff" 
==> "CollectExaples"
==> "CollectRedistGrammars"
==> "GenVersionFile"
==> "ZipAll"

version := getBuildParam "ver"

// Run
Run "ZipAll"