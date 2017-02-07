namespace Yard.Generators.GLL

open Mono.Addins
open Yard.Core
open IL
open Constraints
open Yard.Generators.Common
open InitialConvert
open Yard.Generators.Common.FSA
open Yard.Generators.GLL
open Printer
open Yard.Generators.Common.FSA.Common

open System.Collections.Generic


[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()
[<Extension>]
type GLLFSA() = 
    inherit Generator()
        override this.Name = "GLLGenerator"
        override this.Constraints = [|noMeta; noBrackets; singleModule|]
        override this.Generate (definition, args) =
            let start = System.DateTime.Now
            let args = args.Split([|' ';'\t';'\n';'\r'|]) |> Array.filter ((<>) "")
            let pairs = Array.zeroCreate <| args.Length / 2
            for i = 0 to pairs.Length - 1 do
                pairs.[i] <- args.[i * 2], args.[i * 2 + 1]

            let getOption name either f =
                match definition.options.TryFind name with
                | Some v -> f v
                | None -> either
            let getBoolOption opt either =
                getOption opt either <| function
                    | "true" -> true
                    | "false" -> false
                    | x -> failwithf "Option %s expected values true or false, but %s found." opt x
            let mapFromType t = Map.ofList ["_", Some t]

            let mutable moduleName = getOption "module" "" id
            let mutable tokenType = getOption "token" definition.tokens mapFromType
            //let mutable fullPath = getBoolOption "fullpath" false
            let mutable positionType = getOption "pos" "" id
            //let mutable needTranslate = getBoolOption "translate" false
            let mutable light = getBoolOption "light" true
            //let mutable printInfiniteEpsilonPath = getOption "infEpsPath" "" id
            let mutable isAbstract = getBoolOption "abstract" true
            //let mutable withoutTree = ref <| getBoolOption "withoutTree" true
            let mutable outFileName =
                let fstVal = getOption "output" (definition.info.fileName + ".fs") id
                getOption "o" fstVal id

            let getBoolValue name = function
                    | "true" -> true
                    | "false" -> false
                    | value -> failwithf "Unexpected %s value %s" name value

            for opt, value in pairs do
                match opt with
                | "-module" -> moduleName <- value
                | "-token" -> tokenType <- mapFromType value
                | "-pos" -> positionType <- value
                | "-o" -> if value.Trim() <> "" then outFileName <- value
                | "-output" -> if value.Trim() <> "" then outFileName <- value
                //| "-fullpath" -> fullPath <- getBoolValue "fullPath" value
                //| "-translate" -> needTranslate <- getBoolValue "translate" value
                | "-light" -> light <- getBoolValue "light" value
                //| "-infEpsPath" -> printInfiniteEpsilonPath <- value
                | "-abstract" -> isAbstract <- getBoolValue "abstract" value
                //| "-withoutTree" -> withoutTree := getBoolValue "withoutTree" value
                | value -> failwithf "Unexpected %s option" value
                 
            let fsa = new FSA(definition.grammar.[0].rules)
            
            use out = new System.IO.StreamWriter (outFileName)
            // TODO: write foot of definition
            let generatedCode = printGLL fsa outFileName tokenType moduleName light isAbstract

            out.WriteLine (generatedCode.ToString().Replace("\r\n", "\n").Replace("\n", "\r\n"))
            out.Flush()
            out.Close()
            eprintfn "Generation time: %A" <| System.DateTime.Now - start
            
            box ()
        override this.Generate definition = this.Generate (definition, "")
