module Solution
    open System.IO
    #load "project.fsx"

    let Common = 
        let p = new Projects.Project()
        p.Name <- "Common.fsproj"
        p.Folder <- @"./Core/Common/"
        p.SvnSrcFolder <- @"../YaccConstructor/Common/"
        p

    let Convertions = 
        let p = new Projects.Project()
        p.Name <- "Convertions.fsproj"
        p.Folder <- @"./Core/Convertions/"
        p.SvnSrcFolder <- @"../YaccConstructor/Convertions/"
        p

    let Main = 
        let p = new Projects.Project()
        p.Name <- "Main.fsproj"
        p.Folder <- @"./Core/Main/"
        p.SvnSrcFolder <- @"../YaccConstructor/Main/"
        p
    
    let Yard = 
        let p = new Projects.Project()
        p.Name <- "YardFrontend.fsproj"
        p.Folder <- @"./Frontends/Yard/"
        p.SvnSrcFolder <- @"../YaccConstructor/YardFrontend/"
        p

    let RACCCommon = 
        let p = new Projects.Project()
        p.Name <- "RACCCommon.fsproj"
        p.Folder <- @"./Generators/RACC/Common/"
        p.SvnSrcFolder <- @"../YaccConstructor/RACCCommon/"
        p

    let RACCCore = 
        let p = new Projects.Project()
        p.Name <- "RACCCore.fsproj"
        p.Folder <- @"./Generators/RACC/Core/"
        p.SvnSrcFolder <- @"../YaccConstructor/RACCCore/"
        p

    let RACCFA = 
        let p = new Projects.Project()
        p.Name <- "RACCFiniteAutomata.fsproj"
        p.Folder <- @"./Generators/RACC/FA/"
        p.SvnSrcFolder <- @"../YaccConstructor/RACCFiniteAutomata/"
        p

    let RACCGenerator = 
        let p = new Projects.Project()
        p.Name <- "RACCGenerator.fsproj"
        p.Folder <- @"./Generators/RACC/Generator/"
        p.SvnSrcFolder <- @"../YaccConstructor/RACCGenerator/"
        p                