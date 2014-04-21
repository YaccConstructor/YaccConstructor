namespace YC.ReSharper.AbstractAnalysis.Plugin.Core

open JetBrains.Application.Progress
open JetBrains.ProjectModel
open JetBrains.ReSharper.Feature.Services.Bulbs
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Files
open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation
open Microsoft.FSharp.Collections
open YC.ReSharper.AbstractAnalysis.Languages
open Yard.Examples.MSParser

type SupportedLangs =
    | Calc
    | TSQL
    | JSON

type Processor(file) =
    let defLang (n:ITreeNode) =
        match n with 
        | :? IInvocationExpression as m ->
            match m.InvocationExpressionReference.GetName().ToLowerInvariant() with
            | "executeimmediate" -> TSQL
            | "eval" -> Calc
            | "objnotation" ->JSON
            | _ -> failwith "Unsupported language for AA!"
        | _ -> failwith "Unexpected information type for language specification!"
    let processLang graph tokenize parse addLError addPError = 
        let tokenize g =
            try 
               tokenize g
               |> Some 
            with
            | Calc.Lexer.LexerError(t,brs) ->
                (t, (brs :?> array<AbstractLexer.Core.Position<ICSharpLiteralExpression>>).[0].back_ref.GetDocumentRange())
                |> addLError
                None
        tokenize graph |> Option.map parse
        |> Option.iter
            (function 
             | Yard.Generators.RNGLR.Parser.Success(_,_) -> ()
             | Yard.Generators.RNGLR.Parser.Error(_,tok,_,_,errors) -> tok |> Array.iter addPError 
            )
        
//(provider: ICSharpContextActionDataProvider) =

    member this.Graphs () =  (new Approximator(file)).Approximate defLang
     
    member this.Process () = 
        let parserErrors = new ResizeArray<_>()
        let lexerErrors = new ResizeArray<_>()
        let filterBrs (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
            let res = new ResizeArray<AbstractLexer.Core.Position<#ITreeNode>>(3)
            brs |> Array.iter(fun br -> if res.Exists(fun x -> obj.ReferenceEquals(x.back_ref,br.back_ref)) |> not then res.Add br)
            res.ToArray()
        //let sourceFile = provider.SourceFile
        //let file = provider.SourceFile.GetPsiServices().Files.GetDominantPsiFile<CSharpLanguage>(sourceFile) :?> ICSharpFile
        let graphs = (new Approximator(file)).Approximate defLang
        let calculatePos (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
            let ranges = 
                brs |> Seq.groupBy (fun x -> x.back_ref)
                |> Seq.map (fun (_, brs) -> brs |> Array.ofSeq)
                |> Seq.map(fun brs ->
                    try
                        let pos =  brs |> Array.map(fun i -> i.pos_cnum)
                        let lengthTok = pos.Length
                        let beginPosTok = pos.[0] + 1
                        let endPosTok = pos.[lengthTok-1] + 2 
                        let endPos = 
                            brs.[0].back_ref.GetDocumentRange().TextRange.EndOffset - endPosTok 
                            - brs.[0].back_ref.GetDocumentRange().TextRange.StartOffset 
                        brs.[0].back_ref.GetDocumentRange().ExtendLeft(-beginPosTok).ExtendRight(-endPos)
                    with
                    | e -> 
                        brs.[0].back_ref.GetDocumentRange())
            ranges

        let addError tok =
            let e t l (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) = 
                calculatePos brs 
                |> Seq.iter
                    (fun dr -> parserErrors.Add <| ((sprintf "%A(%A)" t l), dr))
            match tok with
            | Calc.AbstractParser.MINUS (l,br) -> e "MINUS" l br
            | Calc.AbstractParser.DIV (l,br) -> e "DIV" l br
            | Calc.AbstractParser.PLUS (l,br) -> e "PLUS" l br
            | Calc.AbstractParser.NUMBER (l,br) -> e "NUMBER" l br
            | Calc.AbstractParser.LBRACE (l,br) -> e "LBRACE" l br
            | Calc.AbstractParser.RBRACE (l,br) -> e "RBRACE" l br
            | Calc.AbstractParser.POW (l,br) -> e "POW" l br
            | Calc.AbstractParser.RNGLR_EOF (l,br) -> e "EOF" l br
            | Calc.AbstractParser.ERROR (l,br) -> e "ERROR" l br
            | Calc.AbstractParser.MULT (l,br) -> e "MULT" l br
        
        let addErrorJSON tok = 
            let e t l (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) = 
                calculatePos brs 
                |> Seq.iter (fun dr -> parserErrors.Add <| ((sprintf "%A(%A)" t l), dr))
            match tok with
            | JSON.Parser.NUMBER (l,br) -> e "NUMBER" l br
            | JSON.Parser.STRING1 (l,br) -> e "STRING1" l br
            | JSON.Parser.``L 15`` (br1)
            | JSON.Parser.``L 16`` (br1)
            | JSON.Parser.``L 17`` (br1)
            | JSON.Parser.``L 18`` (br1)
            | JSON.Parser.``L 19`` (br1)
            | JSON.Parser.``L 20`` (br1)
            | JSON.Parser.``L 21`` (br1)
            | JSON.Parser.``L 22`` (br1)
            | JSON.Parser.``L 23`` (br1) ->  
                    let name = (JSON.Parser.tokenToNumber >> JSON.Parser.numToString) tok
                    e name name (fst br1)

        
        let addErrorTSQL tok =
            let e t l (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
                calculatePos brs 
                |> Seq.iter (fun dr -> parserErrors.Add <| ((sprintf "%A(%A)" t l), dr))
            match tok with
            | DEC_NUMBER (sourceText,brs)   -> e "DEC_NUMBER" sourceText.text brs
            | DOUBLE_COLON (sourceText,brs) -> e "DOUBLE_COLON" sourceText.text brs
            | GLOBALVAR (sourceText,brs)    -> e "GLOBALVAR" sourceText.text brs
            | IDENT (sourceText,brs)        -> e "IDENT" sourceText.text brs
            | LOCALVAR (sourceText,brs)     -> e "LOCALVAR" sourceText.text brs
            | RNGLR_EOF (sourceText,brs)-> e "EOF" sourceText.text brs
            | STOREDPROCEDURE (sourceText,brs) -> e "STOREDPROCEDURE" sourceText.text brs
            | STRING_CONST (sourceText,brs) -> e "STRING_CONST" sourceText.text brs
            | WEIGHT (sourceText,brs) -> e "WEIGHT" sourceText.text brs
            | ``L 796`` (brs1) 
            | ``L 797`` (brs1)
            | ``L 798`` (brs1)
            | ``L 799`` (brs1)
            | ``L 800`` (brs1)
            | ``L 801`` (brs1)
            | ``L 802`` (brs1)
            | ``L 803`` (brs1)
            | ``L 804`` (brs1)
            | ``L 805`` (brs1)
            | ``L 806`` (brs1)
            | ``L 807`` (brs1)
            | ``L 808`` (brs1)
            | ``L 809`` (brs1)
            | ``L 810`` (brs1)
            | ``L 811`` (brs1)
            | ``L 812`` (brs1)
            | ``L 813`` (brs1)
            | ``L 814`` (brs1)
            | ``L 815`` (brs1)
            | ``L 816`` (brs1)
            | ``L 817`` (brs1)
            | ``L 818`` (brs1)
            | ``L 819`` (brs1)
            | ``L 820`` (brs1)
            | ``L 821`` (brs1)
            | ``L 822`` (brs1)
            | ``L 823`` (brs1)
            | ``L 824`` (brs1)
            | ``L 825`` (brs1)
            | ``L 826`` (brs1)
            | ``L 827`` (brs1)
            | ``L 828`` (brs1)
            | ``L 829`` (brs1)
            | ``L 830`` (brs1)
            | ``L 831`` (brs1)
            | ``L 832`` (brs1)
            | ``L 833`` (brs1)
            | ``L 834`` (brs1)
            | ``L 835`` (brs1)
            | ``L 836`` (brs1)
            | ``L 837`` (brs1)
            | ``L 838`` (brs1)
            | ``L 839`` (brs1)
            | ``L 840`` (brs1)
            | ``L 841`` (brs1)
            | ``L 842`` (brs1)
            | ``L 843`` (brs1)
            | ``L 844`` (brs1)
            | ``L 845`` (brs1)
            | ``L 846`` (brs1)
            | ``L 847`` (brs1)
            | ``L 848`` (brs1)
            | ``L 849`` (brs1)
            | ``L 850`` (brs1)
            | ``L 851`` (brs1)
            | ``L 852`` (brs1)
            | ``L 853`` (brs1)
            | ``L 854`` (brs1)
            | ``L 855`` (brs1)
            | ``L 856`` (brs1)
            | ``L 857`` (brs1)
            | ``L 858`` (brs1)
            | ``L 859`` (brs1)
            | ``L 860`` (brs1)
            | ``L 861`` (brs1)
            | ``L 862`` (brs1)
            | ``L 863`` (brs1)
            | ``L 864`` (brs1)
            | ``L 865`` (brs1)
            | ``L 866`` (brs1)
            | ``L 867`` (brs1)
            | ``L 868`` (brs1)
            | ``L 869`` (brs1)
            | ``L 870`` (brs1)
            | ``L 871`` (brs1)
            | ``L 872`` (brs1)
            | ``L 873`` (brs1)
            | ``L 874`` (brs1)
            | ``L 875`` (brs1)
            | ``L 876`` (brs1)
            | ``L 877`` (brs1)
            | ``L 878`` (brs1)
            | ``L 879`` (brs1)
            | ``L 880`` (brs1)
            | ``L 881`` (brs1)
            | ``L 882`` (brs1)
            | ``L 883`` (brs1)
            | ``L 884`` (brs1)
            | ``L 885`` (brs1)
            | ``L 886`` (brs1)
            | ``L 887`` (brs1)
            | ``L 888`` (brs1)
            | ``L 889`` (brs1)
            | ``L 890`` (brs1)
            | ``L 891`` (brs1)
            | ``L 892`` (brs1)
            | ``L 893`` (brs1)
            | ``L 894`` (brs1)
            | ``L 895`` (brs1)
            | ``L 896`` (brs1)
            | ``L 897`` (brs1)
            | ``L 898`` (brs1)
            | ``L 899`` (brs1)
            | ``L 900`` (brs1)
            | ``L 901`` (brs1)
            | ``L 902`` (brs1)
            | ``L 903`` (brs1)
            | ``L 904`` (brs1)
            | ``L 905`` (brs1)
            | ``L 906`` (brs1)
            | ``L 907`` (brs1)
            | ``L 908`` (brs1)
            | ``L 909`` (brs1)
            | ``L 910`` (brs1)
            | ``L 911`` (brs1)
            | ``L 912`` (brs1)
            | ``L 913`` (brs1)
            | ``L 914`` (brs1)
            | ``L 915`` (brs1)
            | ``L 916`` (brs1)
            | ``L 917`` (brs1)
            | ``L 918`` (brs1)
            | ``L 919`` (brs1)
            | ``L 920`` (brs1)
            | ``L 921`` (brs1)
            | ``L 922`` (brs1)
            | ``L 923`` (brs1)
            | ``L 924`` (brs1)
            | ``L 925`` (brs1)
            | ``L 926`` (brs1)
            | ``L 927`` (brs1)
            | ``L 928`` (brs1)
            | ``L 929`` (brs1)
            | ``L 930`` (brs1)
            | ``L 931`` (brs1)
            | ``L 932`` (brs1)
            | ``L 933`` (brs1)
            | ``L 934`` (brs1)
            | ``L 935`` (brs1)
            | ``L 936`` (brs1)
            | ``L 937`` (brs1)
            | ``L 938`` (brs1)
            | ``L 939`` (brs1)
            | ``L 940`` (brs1)
            | ``L 941`` (brs1)
            | ``L 942`` (brs1)
            | ``L 943`` (brs1)
            | ``L 944`` (brs1)
            | ``L 945`` (brs1)
            | ``L 946`` (brs1)
            | ``L 947`` (brs1)
            | ``L 948`` (brs1)
            | ``L 949`` (brs1)
            | ``L 950`` (brs1)
            | ``L 951`` (brs1)
            | ``L 952`` (brs1)
            | ``L 953`` (brs1)
            | ``L 954`` (brs1)
            | ``L 955`` (brs1)
            | ``L 956`` (brs1)
            | ``L 957`` (brs1)
            | ``L 958`` (brs1)
            | ``L 959`` (brs1)
            | ``L 960`` (brs1)
            | ``L 961`` (brs1)
            | ``L 962`` (brs1)
            | ``L 963`` (brs1)
            | ``L 964`` (brs1)
            | ``L 965`` (brs1)
            | ``L 966`` (brs1)
            | ``L 967`` (brs1)
            | ``L 968`` (brs1)
            | ``L 969`` (brs1)
            | ``L 970`` (brs1)
            | ``L 971`` (brs1)
            | ``L 972`` (brs1)
            | ``L 973`` (brs1)
            | ``L 974`` (brs1)
            | ``L 975`` (brs1)
            | ``L 976`` (brs1)
            | ``L 977`` (brs1)
            | ``L 978`` (brs1)
            | ``L 979`` (brs1)
            | ``L 980`` (brs1)
            | ``L 981`` (brs1)
            | ``L 982`` (brs1)
            | ``L 983`` (brs1)
            | ``L 984`` (brs1)
            | ``L 985`` (brs1)
            | ``L 986`` (brs1)
            | ``L 987`` (brs1)
            | ``L 988`` (brs1)
            | ``L 989`` (brs1)
            | ``L 990`` (brs1)
            | ``L 991`` (brs1)
            | ``L 992`` (brs1)
            | ``L 993`` (brs1)
            | ``L 994`` (brs1)
            | ``L 995`` (brs1)
            | ``L 996`` (brs1)
            | ``L 997`` (brs1)
            | ``L 998`` (brs1)
            | ``L 999`` (brs1)
            | ``L 1000``(brs1)
            | ``L 1001``(brs1)
            | ``L 1002``(brs1)
            | ``L 1003``(brs1)
            | ``L 1004``(brs1)
            | ``L 1005``(brs1)
            | ``L 1006``(brs1)
            | ``L 1007``(brs1)
            | ``L 1008``(brs1)
            | ``L 1009``(brs1)
            | ``L 1010``(brs1)
            | ``L 1011``(brs1)
            | ``L 1012``(brs1)
            | ``L 1013``(brs1)
            | ``L 1014``(brs1)
            | ``L 1015``(brs1)
            | ``L 1016``(brs1)
            | ``L 1017``(brs1)
            | ``L 1018``(brs1)
            | ``L 1019``(brs1)
            | ``L 1020``(brs1)
            | ``L 1021``(brs1)
            | ``L 1022``(brs1)
            | ``L 1023``(brs1)
            | ``L 1024``(brs1)
            | ``L 1025``(brs1)
            | ``L 1026``(brs1)
            | ``L 1027``(brs1)
            | ``L 1028``(brs1)
            | ``L 1029``(brs1)
            | ``L 1030``(brs1)
            | ``L 1031``(brs1)
            | ``L 1032``(brs1)
            | ``L 1033``(brs1)
            | ``L 1034``(brs1)
            | ``L 1035``(brs1)
            | ``L 1036``(brs1)
            | ``L 1037``(brs1)
            | ``L 1038``(brs1)
            | ``L 1039``(brs1)
            | ``L 1040``(brs1)
            | ``L 1041``(brs1)
            | ``L 1042``(brs1)
            | ``L 1043``(brs1)
            | ``L 1044``(brs1)
            | ``L 1045``(brs1)
            | ``L 1046``(brs1)
            | ``L 1047``(brs1)
            | ``L 1048``(brs1)
            | ``L 1049``(brs1)
            | ``L 1050``(brs1)
            | ``L 1051``(brs1)
            | ``L 1052``(brs1)
            | ``L 1053``(brs1)
            | ``L 1054``(brs1)
            | ``L 1055``(brs1)
            | ``L 1056``(brs1)
            | ``L 1057``(brs1)
            | ``L 1058``(brs1)
            | ``L 1059``(brs1)
            | ``L 1060``(brs1)
            | ``L 1061``(brs1)
            | ``L 1062``(brs1)
            | ``L 1063``(brs1)
            | ``L 1064``(brs1)
            | ``L 1065``(brs1)
            | ``L 1066``(brs1)
            | ``L 1067``(brs1)
            | ``L 1068``(brs1)
            | ``L 1069``(brs1)
            | ``L 1070``(brs1)
            | ``L 1071``(brs1)
            | ``L 1072``(brs1)
            | ``L 1073``(brs1)
            | ``L 1074``(brs1)
            | ``L 1075``(brs1)
            | ``L 1076``(brs1)
            | ``L 1077``(brs1)
            | ``L 1078``(brs1)
            | ``L 1079``(brs1)
            | ``L 1080``(brs1)
            | ``L 1081``(brs1)
            | ``L 1082``(brs1)
            | ``L 1083``(brs1)
            | ``L 1084``(brs1)
            | ``L 1085``(brs1)
            | ``L 1086``(brs1)
            | ``L 1087``(brs1)
            | ``L 1088``(brs1)
            | ``L 1089``(brs1)
            | ``L 1090``(brs1)
            | ``L 1091``(brs1)
            | ``L 1092``(brs1)
            | ``L 1093``(brs1)
            | ``L 1094``(brs1)
            | ``L 1095``(brs1)
            | ``L 1096``(brs1)
            | ``L 1097``(brs1)
            | ``L 1098``(brs1)
            | ``L 1099``(brs1)
            | ``L 1100``(brs1)
            | ``L 1101``(brs1)
            | ``L 1102``(brs1)
            | ``L 1103``(brs1)
            | ``L 1104``(brs1)
            | ``L 1105``(brs1)
            | ``L 1106``(brs1)
            | ``L 1107``(brs1)
            | ``L 1108``(brs1)
            | ``L 1109``(brs1)
            | ``L 1110``(brs1)
            | ``L 1111``(brs1)
            | ``L 1112``(brs1) ->  
                let name = TSQL.getTokenName tok
                e name name (snd brs1)


        graphs
        |> ResizeArray.iter 
            (fun (l,g) ->
                match l with
                | Calc -> processLang g Calc.tokenize Calc.parse lexerErrors.Add  addError
                | TSQL -> processLang g TSQL.tokenize TSQL.parse lexerErrors.Add  addErrorTSQL
                | JSON -> processLang g JSON.tokenize JSON.parse lexerErrors.Add  addErrorJSON )

        lexerErrors,parserErrors