#I "e:/fake/fake"
#r "FakeLib.dll"

open Fake

type Frontend =
   | YARD
   | Irony
   | ANTLR

type Generator =
   | RACC
   | FParsec
   | YardPrinter
   | TreeDump

type DeployType = 
   | Full
   | Custom of List<Frontend>*List<Generator>
   | Develop

let task = Custom([YARD],[RACC])

let version = "0.0.0.500"

let buildDir  = @".\build\"
let deployDir  = @".\deploy\"

let targetDescription = ref "full"

let yacccRoot =  @".\YaccConstructor\"

let getFEPath fe =
    match fe with
    | YARD -> yacccRoot + @"YardFrontend\"  

let getGenPath gen =
    match gen with
    | RACC -> yacccRoot + @"RACC**\"

let appReferences  = 
    match task with
    | Develop -> 
        !+ @".\YaccConstructor\**\*.fsproj" 
        |> Scan
    | Full ->  
        !+ @".\YaccConstructor\**\*.fsproj" 
         -- @".\YaccConstructor\**.Tests\*.fsproj"
         -- @".\YaccConstructor\**.Test\*.fsproj"
         -- @".\YaccConstructor\Tester\*.fsproj"
         -- @".\YaccConstructor\RACCTester\*.fsproj"
        |> Scan
    | Custom(fes, gens) ->
        !+ (yacccRoot + @"Main\*.fsproj"
        |> fun x -> 
             List.fold (fun buf fe -> buf ++ (getFEPath fe + "*.fsproj")) x fes
        |> fun x -> 
             List.fold (fun buf gen -> buf ++ (getGenPath gen + "*.fsproj")) x gens
        |> Scan

Target? Clean <-
    fun _ -> CleanDirs [buildDir; deployDir]


Target? BuildApp <-
    fun _ -> 
        MSBuildRelease buildDir "Build" appReferences
        |> Log "AppBuild-Output: "

Target? Deploy <-
    fun _ ->
        !+ (buildDir + "\**\*.dll") 
         ++ (buildDir + "\**\*.exe") 
         -- "*.zip" 
        |> Scan
        |> Zip buildDir (deployDir + "YaccComstructor." + !targetDescription + "."  + version + ".zip")

Target? Default <- DoNothing

// Dependencies
For? BuildApp <- Dependency? Clean    
For? Deploy <- Dependency? BuildApp      
For? Default <- Dependency? Deploy

Run? Default