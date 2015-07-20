//This module prints the information that is needed for highlighting

module PrintTreeNode

open System
open System.IO
open System.Text

open Yard.Generators.Common

type TokenKind = 
    | Terminal 
    | Literal
    | NonTerminal

type TokenInfo = 
    {
        _namespace : string
        _name : string
        _lang : string
        _type : TokenKind
        _baseClass : string
        _number : int
    }

type FormatPrinter() = 
    let mutable strBuilder = null

    do strBuilder <- new StringBuilder()

    member this.GetString() = strBuilder.ToString()
        
    member this.Print x = 
        Printf.kprintf (fun s -> strBuilder.Append s |> ignore) x

    member this.PrintBr (x : 'a) =
        Printf.kprintf (fun s -> strBuilder.Append(s).Append(System.Environment.NewLine) |> ignore) x

    member this.PrintBrInd num (x : 'a) =
        this.Print "%s" (String.replicate (num <<< 2) " ")
        this.PrintBr x


let nonTermSuffix = "NonTermNode"
let termSuffix = "TermNode"
let literalSuffix = "LitNode"
let baseClassSuffix = "BaseTreeNode"
let csExtension = ".cs"
let xmlExtension = ".xml"
let zoneMarker = "ZoneMarker"

let generateFile path text = 
    use out = new StreamWriter(path : string)
    out.WriteLine(text : string)
    out.Close()

let toClassName (str : string) = 
    let symbols = [| 
                    for i = 0 to str.Length - 1 do
                        if i = 0 
                        then yield Char.ToUpper str.[0]
                        else yield str.[i] 
                    |] 
    new String(symbols)

let getSuffix tokenKind = 
    match tokenKind with
    | Terminal -> termSuffix
    | Literal -> literalSuffix
    | NonTerminal -> nonTermSuffix

//Print ITreeNode implementation
let printBaseTreeNode (nameOfNamespace : string) (nameOfClass : string) (lang : string) = 
    
    let printer = new FormatPrinter()

    printer.PrintBrInd 0 "using System.Collections.Generic;"
    printer.PrintBrInd 0 "using System.Text;"
    printer.PrintBrInd 0 "using System.Linq;"

    printer.PrintBrInd 0 "using JetBrains.DocumentModel;"
    printer.PrintBrInd 0 "using JetBrains.ReSharper.Psi;"
    printer.PrintBrInd 0 "using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;"
    printer.PrintBrInd 0 "using JetBrains.ReSharper.Psi.Impl;"
    printer.PrintBrInd 0 "using JetBrains.ReSharper.Psi.Modules;"
    printer.PrintBrInd 0 "using JetBrains.ReSharper.Psi.Tree;"
    printer.PrintBrInd 0 "using JetBrains.Text;"
    printer.PrintBrInd 0 "using JetBrains.Util;"
    printer.PrintBrInd 0 "using ReSharperExtension.YcIntegration;"

    printer.PrintBr "" 

    printer.PrintBrInd 0 "namespace %s" nameOfNamespace
    printer.PrintBrInd 0 "{"

    printer.PrintBrInd 1 "public class %s : ITreeNode" nameOfClass
    printer.PrintBrInd 1 "{"

    printer.PrintBrInd 2 "private NodeUserDataHolder dataHolder = new NodeUserDataHolder();"
    printer.PrintBr ""
    printer.PrintBrInd 2 "#region ITreeNode members"
    printer.PrintBr ""
    

    printer.PrintBrInd 2 "public ITreeNode Parent"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "get { return PersistentUserData.GetData(Constants.Parent); }"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""
    printer.PrintBrInd 2 "public ITreeNode FirstChild"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "get { return PersistentUserData.GetData(Constants.FirstChild); }"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""
    printer.PrintBrInd 2 "public ITreeNode LastChild"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "get { return PersistentUserData.GetData(Constants.LastChild); }"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""
    printer.PrintBrInd 2 "public ITreeNode NextSibling"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "get { return PersistentUserData.GetData(Constants.NextSibling); }"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""
    printer.PrintBrInd 2 "public ITreeNode PrevSibling"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "get { return PersistentUserData.GetData(Constants.PrevSibling); }"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""
    printer.PrintBrInd 2 "public NodeType NodeType"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "get { return PersistentUserData.GetData(Constants.NodeType); }"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""
    printer.PrintBrInd 2 "public PsiLanguageType Language"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "get { return PersistentUserData.GetData(Constants.Language) ?? UnknownLanguage.Instance; }"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""
    printer.PrintBrInd 2 "public NodeUserData UserData { get; private set; }"
    printer.PrintBrInd 2 "public NodeUserData PersistentUserData { get; private set; }"

    printer.PrintBr ""

    printer.PrintBrInd 2 "public IPsiServices GetPsiServices()"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "return default(IPsiServices);"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public IPsiModule GetPsiModule()"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "return default(IPsiModule);"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public IPsiSourceFile GetSourceFile()"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "return default(IPsiSourceFile);"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public ReferenceCollection GetFirstClassReferences()"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "return ReferenceCollection.Empty;"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public void ProcessDescendantsForResolve(IRecursiveElementProcessor processor)"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "return;"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public T GetContainingNode<T>(bool returnThis = false) where T : ITreeNode"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "return default(T);"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public bool Contains(ITreeNode other)"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "if (this.FirstChild != null)"
    printer.PrintBrInd 4 "return this.Children().Contains(other);"
    printer.PrintBrInd 3 "else"
    printer.PrintBrInd 4 "return this == other;"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public bool IsPhysical()"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "return true;"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public bool IsValid()"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "return true;"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public bool IsFiltered()"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "return true;"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "private int curRange = 0;"
    printer.PrintBrInd 2 "//Calls by external code"
    printer.PrintBrInd 2 "public DocumentRange GetNavigationRange()"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "List<DocumentRange> ranges = UserData.GetData(Constants.Ranges);"
    printer.PrintBrInd 3 "if (ranges == null || ranges.Count == 0)"
    printer.PrintBrInd 4 "return default(DocumentRange);"
    printer.PrintBr ""
    printer.PrintBrInd 3 "if (curRange >= ranges.Count)"
    printer.PrintBrInd 4 "curRange = 0;"
    printer.PrintBrInd 3 "return ranges[curRange++];"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public TreeOffset GetTreeStartOffset()"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "List<DocumentRange> ranges = UserData.GetData(Constants.Ranges);"
    printer.PrintBrInd 3 "if (ranges == null || ranges.Count == 0)"
    printer.PrintBrInd 4 "return TreeOffset.InvalidOffset;"
    printer.PrintBr ""
    printer.PrintBrInd 3 "return new TreeOffset(ranges[0].TextRange.StartOffset);"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public int GetTextLength()"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "return GetText(new StringBuilder()).Length;"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public StringBuilder GetText(StringBuilder to)"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "List<DocumentRange> ranges = UserData.GetData(Constants.Ranges);"
    printer.PrintBrInd 3 "foreach (DocumentRange range in ranges)"
    printer.PrintBrInd 3 "{"
    printer.PrintBrInd 4 "to.Append(range.GetText());"
    printer.PrintBrInd 3 "}"
    printer.PrintBrInd 3 "return to;"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public IBuffer GetTextAsBuffer()"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "return new StringBuffer(GetText());"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public string GetText()"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "return GetText(new StringBuilder()).ToString();"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public ITreeNode FindNodeAt(TreeTextRange treeTextRange)"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "IDocument doc = UserData.GetData(Constants.Document);"
    printer.PrintBrInd 3 "var needRange = new DocumentRange(doc, GetTextRange(treeTextRange));"
    printer.PrintBrInd 3 "List<DocumentRange> ranges = UserData.GetData(Constants.Ranges);"
    printer.PrintBr  ""
    printer.PrintBrInd 3 "bool exists = ranges.Exists(range => range.Contains(needRange));"
    printer.PrintBr ""
    printer.PrintBrInd 3 "if (!exists)"
    printer.PrintBrInd 4 "return null;"
    printer.PrintBr ""
    printer.PrintBrInd 3 "if (FirstChild == null)"
    printer.PrintBrInd 4 "return this;"
    printer.PrintBr ""
    printer.PrintBrInd 3 "for (ITreeNode child = this.FirstChild; child != null; child = child.NextSibling)"
    printer.PrintBrInd 3 "{"
    printer.PrintBrInd 4 "ITreeNode node = child.FindNodeAt(treeTextRange);"
    printer.PrintBrInd 4 "if (node != null)"
    printer.PrintBrInd 5 "return node;"
    printer.PrintBrInd 3 "}"
    printer.PrintBr ""
    printer.PrintBrInd 3 "return null;"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public ICollection<ITreeNode> FindNodesAt(TreeOffset treeTextOffset)"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "return default(ICollection<ITreeNode>);"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public ITreeNode FindTokenAt(TreeOffset treeTextOffset)"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "return null;"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "#endregion"
    printer.PrintBr ""

    printer.PrintBrInd 2 "public %s (string ycTokName, int ycTokNumber)" nameOfClass
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "UserData = dataHolder.GetNodeUserData(this);"
    printer.PrintBrInd 3 "PersistentUserData = dataHolder.GetNodePersistentUserData(this);"
    printer.PrintBr ""
    printer.PrintBrInd 3 "UserData.PutData(Constants.YcTokenName, ycTokName);"
    printer.PrintBrInd 3 "UserData.PutData(Constants.YcTokNumber, ycTokNumber.ToString());"
    printer.PrintBrInd 3 "UserData.PutData(Constants.YcLanguage, \"%s\");" <| lang.ToLowerInvariant()
    printer.PrintBrInd 2 "}"
    
    printer.PrintBr ""
    printer.PrintBrInd 2 "public %s (string ycTokName, int ycTokNumber, IEnumerable<DocumentRange> positions) : this (ycTokName, ycTokNumber)" nameOfClass
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "var ranges = positions.ToList();"
    printer.PrintBrInd 3 "if (ranges.Count > 0)"
    printer.PrintBrInd 3 "{"
    printer.PrintBrInd 4 "UserData.PutData(Constants.Document, ranges[0].Document);"
    printer.PrintBrInd 4 "UserData.PutData(Constants.Ranges, ranges);"
    printer.PrintBrInd 3 "}"
    printer.PrintBrInd 2 "}"
    printer.PrintBr ""

    printer.PrintBrInd 2 "private static TextRange GetTextRange(TreeTextRange treeTextRange)"
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 3 "return new TextRange(treeTextRange.StartOffset.Offset, treeTextRange.EndOffset.Offset);"
    printer.PrintBrInd 2 "}"

    printer.PrintBrInd 1 "}"
    printer.PrintBrInd 0 "}"
    printer.GetString()

let printTreeNode (tokenInfo : TokenInfo) = 
    let printer = new FormatPrinter()

    printer.PrintBrInd 0 "using System.Collections.Generic;"
    printer.PrintBrInd 0 "using JetBrains.DocumentModel;"
    printer.PrintBrInd 0 "using ReSharperExtension.YcIntegration;"
    printer.PrintBrInd 0 ""

    printer.PrintBrInd 0 "namespace %s" tokenInfo._namespace
    printer.PrintBrInd 0 "{"

    let className = 
        let suffix = getSuffix tokenInfo._type
        toClassName tokenInfo._name + suffix

    printer.PrintBrInd 1 "public class %s : %s" className tokenInfo._baseClass
    printer.PrintBrInd 1 "{"

    printer.PrintBrInd 2 "private static string ycTokName = \"%s\";" <| tokenInfo._name.ToLowerInvariant()
    printer.PrintBrInd 2 "private static int ycTokNumber = %d;" <| tokenInfo._number
    printer.PrintBr ""
    printer.PrintBrInd 2 "public %s (IEnumerable<DocumentRange> positions)" className
    printer.PrintBrInd 3 ": base(ycTokName, ycTokNumber, positions)"
    printer.PrintBrInd 2 "{"

    match tokenInfo._type with
    | Literal 
    | Terminal -> printer.PrintBrInd 3 "YcHelper.AddYcItem(ycTokName, ycTokNumber, \"%s\");" <| tokenInfo._lang.ToLowerInvariant()
    | _ -> ()
    printer.PrintBrInd 2 "}"

    printer.PrintBrInd 0 ""
    printer.PrintBrInd 2 "public %s() : base(ycTokName, ycTokNumber)" className
    printer.PrintBrInd 2 "{"
    printer.PrintBrInd 2 "}"

    printer.PrintBrInd 1 "}"
    printer.PrintBrInd 0 "}"
    printer.GetString()

let generateTreeNodeFile folder tokenInfo = 
    let className = 
        let suffix = getSuffix tokenInfo._type
        toClassName <| tokenInfo._name + suffix

    let path = folder + className + csExtension
    let text = printTreeNode tokenInfo
    generateFile path text

//Prints .xml file that contains information about token to color mapping.
let printXML (nameOfNamespace : string) tokens = 

    let printer = new FormatPrinter()

    let availableColors = 
        [
            "ANALYSIS_ERROR_ERRORSTRIPE";
            "ANALYSIS_SUGGESTION_ERRORSTRIPE";
            "ANALYSIS_WARNING_ERRORSTRIPE";
            "CONSTANT_IDENTIFIER_ATTRIBUTE";
            "DEADCODE_ATTRIBUTE";
            "EVENT_IDENTIFIER_ATTRIBUTE";
            "EXTENSION_METHOD_IDENTIFIER_ATTRIBUTE";
            "FIELD_IDENTIFIER_ATTRIBUTE";
            "FORMAT_STRING_ITEM";
            "JAVA_SCRIPT_XML_DOC_TAG";
            "JS_FUNCTION_IDENTIFIER_ATTRIBUTE";
            "JS_LATEBOUND_IDENTIFIER_ATTRIBUTE";
            "JS_LOCAL_IDENTIFIER_ATTRIBUTE";
            "JS_PARAMETER_IDENTIFIER_ATTRIBUTE";
            "JS_PROPERTY_IDENTIFIER_ATTRIBUTE";
            "LATE_BOUND_IDENTIFIER_ATTRIBUTE";
            "LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE";
            //"MATCHED_BRACE";
            "MATCHED_FORMAT_STRING_ITEM";
            "METHOD_IDENTIFIER_ATTRIBUTE";
            "MUTABLE_LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE";
            "NAMESPACE_IDENTIFIER_ATTRIBUTE";
            "OPERATOR_IDENTIFIER_ATTRIBUTE";
            //"OUTLINE_BRACE";
            "PARAMETER_IDENTIFIER_ATTRIBUTE";
            "PATH_IDENTIFIER_ATTRIBUTE";
            "PUBLIC_DEADCODE_ATTRIBUTE";
            "TODOITEM_ATTRIBUTE";
            "TODOITEM_ERRORSTRIPE_ATTRIBUTE";
            "TS_CLASS_IDENTIFIER_ATTRIBUTE";
            "TS_ENUM_IDENTIFIER_ATTRIBUTE";
            "TS_INTERFACE_IDENTIFIER_ATTRIBUTE";
            "TS_MODULE_IDENTIFIER_ATTRIBUTE";
            "TS_TYPE_PARAMETER_IDENTIFIER_ATTRIBUTE";
            "TYPE_CLASS_ATTRIBUTE";
            "TYPE_DELEGATE_ATTRIBUTE";
            "TYPE_ENUM_ATTRIBUTE";
            "TYPE_INTERFACE_ATTRIBUTE";
            "TYPE_PARAMETER_ATTRIBUTE";
            "TYPE_STATIC_CLASS_ATTRIBUTE";
            "TYPE_STRUCT_ATTRIBUTE";
            "UNMATCHED_BRACE";
            "UNRESOLVED_ERROR_ATTRIBUTE";
        ]

    printer.PrintBrInd 0 "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    printer.PrintBrInd 0 "<!--"
    printer.PrintBrInd 2 "Available color definitions:"

    availableColors 
    |> List.iter (fun color -> printer.PrintBrInd 1 "%s" color)

    printer.PrintBr ""

    let availableIcons = 
        [
            "CLASS_IS_INHERITED_ATTRIBUTE";
            "HIDES_ATTRIBUTE";
            "IMPLEMENTS_AND_HIDES_ATTRIBUTE";
            "IMPLEMENTS_AND_OVERRIDES_ATTRIBUTE";
            "IMPLEMENTS_ATTRIBUTE";
            "INTERFACE_IS_IMPLEMENTED_ATTRIBUTE";
            "OVERRIDES_ATTRIBUTE";
            "RECURSION_ATTRIBUTE";
        ]

    printer.PrintBrInd 2 "Available icons:"
    availableIcons 
    |> List.iter (fun icon -> printer.PrintBrInd 1 "%s" icon)

    printer.PrintBr ""
    let availableUnderlying = 
        [
            "ERROR_ATTRIBUTE";
            "HINT_ATTRIBUTE";
            "SUGGESTION_ATTRIBUTE";
            "WARNING_ATTRIBUTE";
        ]

    printer.PrintBrInd 2 "Available underlying"
    availableUnderlying
    |> List.iter (fun underlying -> printer.PrintBrInd 1 "%s" underlying)

    printer.PrintBrInd 0 "-->"

    printer.PrintBrInd 0 "<SyntaxDefinition name=\"%s\">" nameOfNamespace
    printer.PrintBrInd 1 "<Colors>"
    printer.PrintBrInd 2 "<Tokens color=\"CONSTANT_IDENTIFIER_ATTRIBUTE\">"

    tokens
    |> List.iter(fun token -> printer.PrintBrInd 3 "<Token> %s </Token>" token)

    printer.PrintBrInd 2 "</Tokens>"
    printer.PrintBrInd 1 "</Colors>"
    
    printer.PrintBrInd 0 "<!-- Dynamic highlighting:"
    printer.PrintBrInd 1 "<Matched>"
    let pair = [
                "LBRACE", "RBRACE"; 
                "LEFT_SQUARE_BRACKET", "RIGHT_SQUARE_BRACKET"; 
                "LEFT_FIGURE_BRACKET", "LEFT_FIGURE_BRACKET"
               ]

    pair 
    |> List.iter 
        (
            fun (left, right) -> 
                printer.PrintBrInd 2 "<Pair>"
                printer.PrintBrInd 3 "<Left> %s </Left>" left
                printer.PrintBrInd 3 "<Right> %s </Right>" right
                printer.PrintBrInd 2 "</Pair>"
        )

    printer.PrintBrInd 1 "</Matched>"
    printer.PrintBrInd 0 "-->"

    printer.PrintBrInd 0 "</SyntaxDefinition>"
    printer.GetString()

//prints "tokenToTreeNode" function in parser file. 
//function "tokenToTreeNode" needs in highlihgting after lexical analysis.
let printTokenToTreeNode (indexator : Indexator) = 
    
    let printer = new FormatPrinter()

    printer.PrintBrInd 0 "let tokenToTreeNode token = "
    printer.PrintBrInd 1 "match token with"
            
    for i = indexator.termsStart to indexator.termsEnd do
        let termNode = toClassName <| indexator.indexToTerm i
        printer.PrintBrInd 1 "| %s data -> " termNode
        printer.PrintBrInd 2 "let ranges = calculatePos data"
        printer.PrintBrInd 2 "new %s%s(ranges) :> ITreeNode" termNode termSuffix

    for i = indexator.literalsStart to indexator.literalsEnd do
        let litNode = toClassName <| indexator.indexToLiteral i
        printer.PrintBrInd 1 "| L_%s data -> " <| indexator.indexToLiteral i
        printer.PrintBrInd 2 "let ranges = calculatePos data"
        printer.PrintBrInd 2 "new %s%s(ranges) :> ITreeNode" litNode literalSuffix

    printer.GetString()

let printZoneMarkerText _namespace = 
    
    let printer = new FormatPrinter()

    printer.PrintBr "using JetBrains.Application.BuildScript.Application.Zones;"
    printer.PrintBr ""

    printer.PrintBr "namespace %s" _namespace
    printer.PrintBr "{"
    printer.PrintBrInd 1 "[ZoneMarker]"
    printer.PrintBrInd 1 "public class %s" zoneMarker
    printer.PrintBrInd 1 "{"
    printer.PrintBrInd 1 "}"
    printer.PrintBr "}"

    printer.GetString()

let printItemsGroup nameOfClasses xmlName = 

    let printer = new FormatPrinter()
    
    printer.PrintBrInd 0 "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    printer.PrintBrInd 0 "<!-- Generate file must be include in .csproj with help follow strings "
    printer.PrintBrInd 0 "<Import Project=\"ItemsGroup.target\" />"
    printer.PrintBrInd 0 "<ItemGroup> <Compile Include=\"@(ExternalCompile)\" /></ItemGroup> -->"
    printer.PrintBrInd 1 "<Project ToolsVersion=\"4.0\" DefaultTargets=\"Build\" xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">"
    printer.PrintBrInd 1 "<ItemGroup>" 
    printer.PrintBrInd 2 "<ExternalCompile Include=\"Properties\AssemblyInfo.cs\" />"
    for className in nameOfClasses do
        printer.PrintBrInd 2 "<ExternalCompile Include=\"%s\" />" className

    printer.PrintBrInd 2 "<Content Include=\"%s%s\">" xmlName xmlExtension
    printer.PrintBrInd 3 "<CopyToOutputDirectory>Always</CopyToOutputDirectory>"
    printer.PrintBrInd 2 "</Content>"

    printer.PrintBrInd 1 "</ItemGroup>"
    printer.PrintBrInd 1 "</Project>"
    printer.GetString()

let generate (indexator : Indexator) namespaceName = 
    let folder = Path.GetFullPath namespaceName + "\\"
    let langName = namespaceName.Replace ("Highlighting", "")
    let baseClass = langName + baseClassSuffix

    let generateXML() = 
        let fileName = folder + baseClass + csExtension
        let text = printBaseTreeNode namespaceName baseClass langName 
        generateFile fileName text
    
    generateXML()

    let mutable tokensAndLits = []
    let nameOfClasses = ref [zoneMarker + csExtension]
                
    for i = 0 to indexator.nonTermCount - 1 do
        let name = toClassName <| indexator.indexToNonTerm i
        if not <| name.Contains ("Highlight_")
        then 
            nameOfClasses := name + nonTermSuffix + csExtension :: !nameOfClasses
            let info : TokenInfo =  
                {
                    _baseClass = baseClass
                    _namespace = namespaceName
                    _name = name
                    _type = TokenKind.NonTerminal
                    _number = i
                    _lang = langName
                }

            generateTreeNodeFile folder info

    for i = indexator.termsStart to indexator.termsEnd do
        let name = indexator.indexToTerm i
                    
        nameOfClasses := name + termSuffix + csExtension :: !nameOfClasses
        tokensAndLits <- name :: tokensAndLits
        let info : TokenInfo =  
            {
                _baseClass = baseClass
                _namespace = namespaceName
                _name = name
                _type = TokenKind.Terminal
                _number = i
                _lang = langName
            }

        generateTreeNodeFile folder info
                
    for i = indexator.literalsStart to indexator.literalsEnd do
        let name = toClassName <| indexator.getLiteralName i
                    
        nameOfClasses := name + literalSuffix + csExtension :: !nameOfClasses
        tokensAndLits <- name :: tokensAndLits
        let info : TokenInfo =  
            {
                _baseClass = baseClass
                _namespace = namespaceName
                _name = name
                _type = TokenKind.Literal
                _number = i
                _lang = langName
            }

        generateTreeNodeFile folder info
                    
    //generateHotspotXMLFile "Hotspots.xml"
    tokensAndLits <- tokensAndLits |> List.rev
    
    let generateXML name toksAndLits = 
        let path = folder + name + xmlExtension
        if not <| File.Exists path
        then 
            let text = printXML name toksAndLits
            generateFile path text
    generateXML namespaceName tokensAndLits
    
    let generateZoneMarkerFile = 
        let path = folder + zoneMarker + csExtension 
        let text = printZoneMarkerText namespaceName
        generateFile path text

    nameOfClasses := !nameOfClasses |> List.rev

    let generateItemGroup() =
        let fileName = folder + "ItemsGroup.target"
        let text = printItemsGroup <| List.rev (baseClass + csExtension :: !nameOfClasses) <| namespaceName
        generateFile fileName text
    
    generateItemGroup()

let printHotspotFile() = 
    
    let printer = new FormatPrinter()

    printer.PrintBrInd 0 "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    printer.PrintBrInd 0 "<Body>"
    printer.PrintBrInd 0 "<!-- enter name of language. For example, TSQL-->"
    printer.PrintBrInd 1 "<Language name=\"\">"
    printer.PrintBrInd 2 "<Hotspot>"
    printer.PrintBrInd 2 "<!-- Format: \"<Class>.<Method>\". For example, \"Program.Eval\" -->"
    printer.PrintBrInd 3 "<Fullname></Fullname>"
    printer.PrintBrInd 3 "<!-- If you call Program.Eval(query, parameters) where variable parameters is parameters of connection, then value ArgumentPosition is 0 (zero-based)-->"
    printer.PrintBrInd 3 "<ArgumentPosition> 0 </ArgumentPosition>"
    printer.PrintBrInd 3 "type of result of query: int, string, void etc"
    printer.PrintBrInd 3 "<ReturnType></ReturnType>"
    printer.PrintBrInd 2 "</Hotspot>"
    printer.PrintBrInd 1 "</Language>"
    printer.GetString()