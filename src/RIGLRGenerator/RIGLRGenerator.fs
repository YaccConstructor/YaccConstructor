namespace Yard.Generators.RIGLRGenerator

open System.Diagnostics
open System.IO
open System.Text

open Mono.Addins
open Yard.Core
open Yard.Generators.Common
open InitialConvert
open Yard.Generators.Common.FinalGrammar
open Constraints
open Automata
open PrintTable

[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()

[<Extension>]
type RIGLR() = 
    inherit Generator()
        override this.Name = "RIGLRGenerator"
        override this.Constraints = [|noEbnf; noMeta; noInnerAlt; (*noLiterals;*) noInnerAlt; noBrackets; needAC; singleModule|]
        override this.Generate (definition, args) =   
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
            let mutable output =
                let fstVal = getOption "output" (definition.info.fileName + ".fs") id
                getOption "o" fstVal id                        

            for opt, value in pairs do
                match opt with
                | "-module" -> moduleName <- value
                | "-token" -> tokenType <- mapFromType value

            let mutable newDefinition = initialConvert definition
            let grammar = new FinalGrammar(newDefinition.grammar.[0].rules, true)                        
            let RCA = new RCA(grammar)                        
            RCA.PrintToDOT("dot.dot", (fun x -> x.ToString()))
            
            let table = RCA.ToTable()
            Set.iter (fun s -> printfn "%A" s) RCA.PopStates
            let res = new StringBuilder()
            let println (x : 'a) =
                Printf.kprintf (fun s -> res.Append(s).Append "\n" |> ignore) x
            let print (x : 'a) =
                Printf.kprintf (fun s -> res.Append(s) |> ignore) x             
            
            println "module %s" moduleName 
            println "open Yard.Generators.RIGLR"
            println "open Yard.Generators.RIGLR.Parser"
            println "open Yard.Generators.Common.AST"                              
            let strTable = printTable grammar table moduleName tokenType (Seq.toList RCA.FinalState)
                                      RCA.PopStates res int false false
            
            use out = new StreamWriter(output) 
            out.WriteLine(strTable)
            out.Close()
            
            box ()
        override this.Generate definition = this.Generate (definition, "")