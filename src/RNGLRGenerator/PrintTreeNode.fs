//This module prints the information that is needed for highlighting

module PrintTreeNode

open Yard.Generators.RNGLR

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

let toClassName (str : string) = 
    let symbols = [| 
                    for i = 0 to str.Length - 1 do
                        if i = 0 
                        then yield System.Char.ToUpper str.[0]
                        else yield str.[i] 
                    |] 
    new System.String(symbols)

let nonTermSuffix = "NonTermNode"
let termSuffix = "TermNode"
let literalSuffix = "LitNode"
let baseClassSuffix = "BaseTreeNode"
let extension = ".cs"

let getSuffix tokenKind = 
    match tokenKind with
    | Terminal -> termSuffix
    | Literal -> literalSuffix
    | NonTerminal -> nonTermSuffix

//Print ITreeNode implementation
let printBaseTreeNode (nameOfNamespace : string) (nameOfClass : string) (lang : string) = 
    let res  = new System.Text.StringBuilder()

    let inline print (x : 'a) =
        Printf.kprintf (fun s -> res.Append s |> ignore) x

    let inline printBr (x : 'a) =
        Printf.kprintf (fun s -> res.Append(s).Append(System.Environment.NewLine) |> ignore) x

    let inline printBrInd num (x : 'a) =
        print "%s" (String.replicate (num <<< 2) " ")
        printBr x

    printBrInd 0 "using System.Collections.Generic;"
    printBrInd 0 "using System.Text;"
    printBrInd 0 "using System.Linq;"
    printBrInd 0 "using JetBrains.DocumentModel;"
    printBrInd 0 "using JetBrains.ReSharper.Psi;"
    printBrInd 0 "using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;"
    printBrInd 0 "using JetBrains.ReSharper.Psi.Modules;"
    printBrInd 0 "using JetBrains.ReSharper.Psi.Tree;"
    //printBrInd 0 "using JetBrains.ReSharper.Psi.Impl;"
    printBrInd 0 "using JetBrains.Text;"
    printBrInd 0 "using Highlighting.Core;"

    printBr "" 

    printBrInd 0 "namespace %s" nameOfNamespace
    printBrInd 0 "{"

    printBrInd 1 "public class %s : ITreeNode" nameOfClass
    printBrInd 1 "{"
    printBrInd 2 "public ITreeNode Parent"
    printBrInd 2 "{"
    printBrInd 3 "get { return PersistentUserData.GetData(PropertyConstant.Parent); }"
    printBrInd 2 "}"
    printBr ""
    printBrInd 2 "public ITreeNode FirstChild"
    printBrInd 2 "{"
    printBrInd 3 "get { return PersistentUserData.GetData(PropertyConstant.FirstChild); }"
    printBrInd 2 "}"
    printBr ""
    printBrInd 2 "public ITreeNode LastChild"
    printBrInd 2 "{"
    printBrInd 3 "get { return PersistentUserData.GetData(PropertyConstant.LastChild); }"
    printBrInd 2 "}"
    printBr ""
    printBrInd 2 "public ITreeNode NextSibling"
    printBrInd 2 "{"
    printBrInd 3 "get { return PersistentUserData.GetData(PropertyConstant.NextSibling); }"
    printBrInd 2 "}"
    printBr ""
    printBrInd 2 "public ITreeNode PrevSibling"
    printBrInd 2 "{"
    printBrInd 3 "get { return PersistentUserData.GetData(PropertyConstant.PrevSibling); }"
    printBrInd 2 "}"
    printBr ""
    printBrInd 2 "public NodeType NodeType"
    printBrInd 2 "{"
    printBrInd 3 "get { return PersistentUserData.GetData(PropertyConstant.NodeType); }"
    printBrInd 2 "}"
    printBr ""
    printBrInd 2 "public PsiLanguageType Language"
    printBrInd 2 "{"
    printBrInd 3 "get { return PersistentUserData.GetData(PropertyConstant.Language) ?? UnknownLanguage.Instance; }"
    printBrInd 2 "}"
    printBr ""
    printBrInd 2 "public NodeUserData UserData { get; private set; }"
    printBrInd 2 "public NodeUserData PersistentUserData { get; private set; }"
//    printBrInd 2 "public NodeUserDataHolder NodeUserDataHolder { get; private set; }"    
    printBr ""
    printBrInd 2 "public %s (string ycTokName) : this (ycTokName, string.Empty)" nameOfClass
    printBrInd 2 "{"
    printBrInd 2 "}"

    printBr ""
    printBrInd 2 "public %s (string ycTokName, string ycValue)" nameOfClass
    printBrInd 2 "{"
    printBrInd 3 "UserData = DataHelper.GetNodeUserData(this);"
    printBrInd 3 "PersistentUserData = DataHelper.GetNodePersistentUserData(this);"
    printBr ""
    printBrInd 3 "UserData.PutData(KeyConstant.YcTokenName, ycTokName);"
    printBrInd 3 "UserData.PutData(KeyConstant.YcTextValue, ycValue);"
    printBrInd 3 "UserData.PutData(KeyConstant.YcLanguage, \"%s\");" <| lang.ToLowerInvariant()
    printBrInd 2 "}"
    
    printBr ""
    printBrInd 2 "public %s (string ycTokName, string ycValue, IEnumerable<DocumentRange> positions) : this (ycTokName, ycValue)" nameOfClass
    printBrInd 2 "{"
//    printBrInd 3 "SetPositions(positions as IEnumerable<DocumentRange>);"
//    printBrInd 2 "}"
//    printBr ""
    // printing all methods

//    printBrInd 2 "private void SetPositions(IEnumerable<DocumentRange> positions)"
//    printBrInd 2 "{"
//    printBrInd 3 "if (positions == null)"
//    printBrInd 4 "return;"
//    printBr ""
    printBrInd 3 "var ranges = positions.ToList();"
    printBrInd 3 "if (ranges.Count > 0)"
    printBrInd 3 "{"
    printBrInd 4 "UserData.PutData(KeyConstant.Document, ranges[0].Document);"
    printBrInd 4 "UserData.PutData(KeyConstant.Ranges, ranges);"
    printBrInd 3 "}"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public IPsiServices GetPsiServices()"
    printBrInd 2 "{"
    printBrInd 3 "return default(IPsiServices);"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public IPsiModule GetPsiModule()"
    printBrInd 2 "{"
    printBrInd 3 "return default(IPsiModule);"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public IPsiSourceFile GetSourceFile()"
    printBrInd 2 "{"
    printBrInd 3 "return default(IPsiSourceFile);"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public ReferenceCollection GetFirstClassReferences()"
    printBrInd 2 "{"
    printBrInd 3 "return ReferenceCollection.Empty;"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public void ProcessDescendantsForResolve(IRecursiveElementProcessor processor)"
    printBrInd 2 "{"
    printBrInd 3 "return;"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public T GetContainingNode<T>(bool returnThis = false) where T : ITreeNode"
    printBrInd 2 "{"
    printBrInd 3 "return default(T);"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public bool Contains(ITreeNode other)"
    printBrInd 2 "{"
    printBrInd 3 "if (this.FirstChild != null)"
    printBrInd 4 "return this.Children().Contains(other);"
    printBrInd 3 "else"
    printBrInd 4 "return this == other;"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public bool IsPhysical()"
    printBrInd 2 "{"
    printBrInd 3 "return true;"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public bool IsValid()"
    printBrInd 2 "{"
    printBrInd 3 "return true;"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public bool IsStub()"
    printBrInd 2 "{"
    printBrInd 3 "return false;"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public bool IsFiltered()"
    printBrInd 2 "{"
    printBrInd 3 "return true;"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "private int curRange = 0;"
    printBrInd 2 "//Calls by external code"
    printBrInd 2 "public DocumentRange GetNavigationRange()"
    printBrInd 2 "{"
    printBrInd 3 "List<DocumentRange> ranges = UserData.GetData(KeyConstant.Ranges);"
    printBrInd 3 "if (ranges == null || ranges.Count == 0)"
    printBrInd 4 "return default(DocumentRange);"
    printBr ""
    printBrInd 3 "if (curRange >= ranges.Count)"
    printBrInd 4 "curRange = 0;"
    printBrInd 3 "return ranges[curRange++];"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public TreeOffset GetTreeStartOffset()"
    printBrInd 2 "{"
    printBrInd 3 "List<DocumentRange> ranges = UserData.GetData(KeyConstant.Ranges);"
    printBrInd 3 "if (ranges == null || ranges.Count == 0)"
    printBrInd 4 "return TreeOffset.InvalidOffset;"
    printBr ""
    printBrInd 3 "return new TreeOffset(ranges[0].TextRange.StartOffset);"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public int GetTextLength()"
    printBrInd 2 "{"
    printBrInd 3 "return GetText(new StringBuilder()).Length;"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public StringBuilder GetText(StringBuilder to)"
    printBrInd 2 "{"
    printBrInd 3 "List<DocumentRange> ranges = UserData.GetData(KeyConstant.Ranges);"
    printBrInd 3 "foreach (DocumentRange range in ranges)"
    printBrInd 3 "{"
    printBrInd 4 "to.Append(range.GetText());"
    printBrInd 3 "}"
    printBrInd 3 "return to;"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public IBuffer GetTextAsBuffer()"
    printBrInd 2 "{"
    printBrInd 3 "return new StringBuffer(GetText());"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public string GetText()"
    printBrInd 2 "{"
    printBrInd 3 "return GetText(new StringBuilder()).ToString();"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public ITreeNode FindNodeAt(TreeTextRange treeTextRange)"
    printBrInd 2 "{"
    printBrInd 3 "IDocument doc = UserData.GetData(KeyConstant.Document);"
    printBrInd 3 "var needRange = new DocumentRange(doc, treeTextRange.GetTextRange());"
    printBrInd 3 "List<DocumentRange> ranges = UserData.GetData(KeyConstant.Ranges);"
    printBr  ""
    printBrInd 3 "bool exists = ranges.Exists(range => range.Contains(needRange));"
    printBr ""
    printBrInd 3 "if (!exists)"
    printBrInd 4 "return null;"
    printBr ""
    printBrInd 3 "if (FirstChild == null)"
    printBrInd 4 "return this;"
    printBr ""
    printBrInd 3 "for (ITreeNode child = this.FirstChild; child != null; child = child.NextSibling)"
    printBrInd 3 "{"
    printBrInd 4 "ITreeNode node = child.FindNodeAt(treeTextRange);"
    printBrInd 4 "if (node != null)"
    printBrInd 5 "return node;"
    printBrInd 3 "}"
    printBr ""
    printBrInd 3 "return null;"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public ICollection<ITreeNode> FindNodesAt(TreeOffset treeTextOffset)"
    printBrInd 2 "{"
    printBrInd 3 "return default(ICollection<ITreeNode>);"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public ITreeNode FindTokenAt(TreeOffset treeTextOffset)"
    printBrInd 2 "{"
    printBrInd 3 "return null;"
    printBrInd 2 "}"
    printBr ""

    printBrInd 1 "}"
    printBrInd 0 "}"
    res.ToString()

let printTreeNode (tokenInfo : TokenInfo) = 
    let res  = new System.Text.StringBuilder()

    let inline print (x : 'a) = Printf.kprintf (fun s -> res.Append s |> ignore) x

    let inline printBr (x : 'a) = Printf.kprintf (fun s -> res.Append(s).Append(System.Environment.NewLine) |> ignore) x

    let inline printBrInd num (x : 'a) = 
        print "%s" (String.replicate (num <<< 2) " ")
        printBr x
    
    printBrInd 0 "using Highlighting.Core;"
    printBrInd 0 "using System.Collections.Generic;"
    printBrInd 0 "using JetBrains.DocumentModel;"
    printBrInd 0 ""

    printBrInd 0 "namespace %s" tokenInfo._namespace
    printBrInd 0 "{"

    let className = 
        let suffix = getSuffix tokenInfo._type
        toClassName tokenInfo._name + suffix

    printBrInd 1 "public class %s : %s" className tokenInfo._baseClass
    printBrInd 1 "{"

    printBrInd 2 "private static string ycTokName = \"%s\";" <| tokenInfo._name.ToLowerInvariant()
    printBr ""
    printBrInd 2 "public %s (string ycValue, IEnumerable<DocumentRange> positions)" className
    printBrInd 3 ": base(ycTokName, ycValue, positions)"
    printBrInd 2 "{"

    match tokenInfo._type with
    | Literal 
    | Terminal -> printBrInd 3 "YcHelper.AddYcItem(ycTokName, ycValue, %d, \"%s\");" tokenInfo._number <| tokenInfo._lang.ToLowerInvariant()
    | _ -> ()
    printBrInd 2 "}"

    printBrInd 0 ""
    printBrInd 2 "public %s() : base(ycTokName)" className
    printBrInd 2 "{"
    printBrInd 2 "}"

    printBrInd 1 "}"
    printBrInd 0 "}"
    res.ToString()

let generateTreeNodeFile folder tokenInfo = 
    let className = 
        let suffix = getSuffix tokenInfo._type
        toClassName <| tokenInfo._name + suffix

    use out = new System.IO.StreamWriter (folder + className + extension)
    let tables = printTreeNode tokenInfo
    out.WriteLine tables
    out.Close()

//Prints .xml file which contains information about token to color mapping.
let printXML (nameOfNamespace : string) tokens = 
    let res  = new System.Text.StringBuilder()

    let inline print (x : 'a) =
        Printf.kprintf (fun s -> res.Append s |> ignore) x

    let inline printBr (x : 'a) =
        Printf.kprintf (fun s -> res.Append(s).Append(System.Environment.NewLine) |> ignore) x

    let inline printBrInd num (x : 'a) =
        print "%s" (String.replicate (num <<< 2) " ")
        printBr x

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

    printBrInd 0 "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    printBrInd 0 "<!--"
    printBrInd 2 "Available color definitions:"

    for color in availableColors do
        printBrInd 1 "%s" color
    printBr ""

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

    printBrInd 2 "Available icons:"
    for icon in availableIcons do
        printBrInd 1 "%s" icon

    printBr ""
    let availableUnderlying = 
        [
            "ERROR_ATTRIBUTE";
            "HINT_ATTRIBUTE";
            "SUGGESTION_ATTRIBUTE";
            "WARNING_ATTRIBUTE";
        ]

    printBrInd 2 "Available underlying"
    for underlying in availableUnderlying do
        printBrInd 1 "%s" underlying

    printBrInd 0 "-->"

    printBrInd 0 "<SyntaxDefinition name=\"%s\">" nameOfNamespace
    printBrInd 1 "<Colors>"
    printBrInd 2 "<Tokens color=\"CONSTANT_IDENTIFIER_ATTRIBUTE\">"

    for tok in tokens do
        printBrInd 3 "<Token> %s </Token>" tok

    printBrInd 2 "</Tokens>"
    printBrInd 1 "</Colors>"
    
    printBrInd 0 "<!-- Dynamic highlighting:"
    printBrInd 1 "<Matched>"
    let pair = [
                "LBRACE", "RBRACE"; 
                "LEFT_SQUARE_BRACKET", "RIGHT_SQUARE_BRACKET"; 
                "LEFT_FIGURE_BRACKET", "LEFT_FIGURE_BRACKET"
               ]

    for left, right in pair do
        printBrInd 2 "<Pair>"
        printBrInd 3 "<Left> %s </Left>" left
        printBrInd 3 "<Right> %s </Right>" right
        printBrInd 2 "</Pair>"
    printBrInd 1 "</Matched>"
    printBrInd 0 "-->"

    printBrInd 0 "</SyntaxDefinition>"
    res.ToString()

//prints "addSemantic" function in parser file.
//function addSemantic sets relations between nodes (father, child, brother)
let printAddSemantic() = 
    let res  = new System.Text.StringBuilder()

    let inline print (x : 'a) =
        Printf.kprintf (fun s -> res.Append s |> ignore) x

    let inline printBr (x : 'a) =
        Printf.kprintf (fun s -> res.Append(s).Append(System.Environment.NewLine) |> ignore) x

    let inline printBrInd num (x : 'a) =
        print "%s" (String.replicate (num <<< 2) " ")
        printBr x

    printBrInd 0 "let addSemantic (parent : ITreeNode) (children : ITreeNode list) = " 
    printBrInd 1 "let mutable prev = null"
    printBrInd 1 "let mutable curr = null"
    printBrInd 1 "let ranges = new ResizeArray<JetBrains.DocumentModel.DocumentRange>()"
    printBrInd 1 "for child in children do"
    printBrInd 2 "prev <- curr"
    printBrInd 2 "curr <- child"
    printBrInd 2 "curr.PersistentUserData.PutData(PropertyConstant.Parent, parent)"
    printBrInd 2 "ranges.AddRange (curr.UserData.GetData(KeyConstant.Ranges))"
    printBrInd 2 "if prev = null"
    printBrInd 2 "then parent.PersistentUserData.PutData(PropertyConstant.FirstChild, curr)"
    printBrInd 2 "else"
    printBrInd 3 "prev.PersistentUserData.PutData(PropertyConstant.NextSibling, curr)"
    printBrInd 3 "curr.PersistentUserData.PutData(PropertyConstant.PrevSibling, prev)"
    printBrInd 1 "parent.PersistentUserData.PutData(PropertyConstant.LastChild, curr)"
    printBrInd 1 "parent.UserData.PutData(KeyConstant.Ranges, ranges)"
    printBrInd 1 "if ranges <> null && ranges.Count > 0"
    printBrInd 1 "then parent.UserData.PutData(KeyConstant.Document, ranges.[0].Document)"
    printBrInd 1 "parent"
    res.ToString()

//prints "calculatePos" function in parser file. 
//function calculatePos returns token coordinates.
let printCalculatePos() = 
    let res  = new System.Text.StringBuilder()

    let inline print (x : 'a) =
        Printf.kprintf (fun s -> res.Append s |> ignore) x

    let inline printBr (x : 'a) =
        Printf.kprintf (fun s -> res.Append(s).Append(System.Environment.NewLine) |> ignore) x

    let inline printBrInd num (x : 'a) =
        print "%s" (String.replicate (num <<< 2) " ")
        printBr x

    printBrInd 0 "let calculatePos (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) ="
    printBrInd 1 "let ranges = "
    printBrInd 2 "brs |> Seq.groupBy (fun x -> x.back_ref)"
    printBrInd 2 "|> Seq.map (fun (_, brs) -> brs |> Array.ofSeq)"
    printBrInd 2 "|> Seq.map(fun brs ->"
    printBrInd 3 "try"
    printBrInd 4 "let pos =  brs |> Array.map(fun i -> i.pos_cnum)"
    printBrInd 4 "let lengthTok = pos.Length"
    printBrInd 4 "let beginPosTok = pos.[0] + 1"
    printBrInd 4 "let endPosTok = pos.[lengthTok-1] + 2"
    printBrInd 4 "let endPos = "
    printBrInd 5 "brs.[0].back_ref.GetDocumentRange().TextRange.EndOffset - endPosTok"
    printBrInd 5 "- brs.[0].back_ref.GetDocumentRange().TextRange.StartOffset"
    printBrInd 4 "brs.[0].back_ref.GetDocumentRange().ExtendLeft(-beginPosTok).ExtendRight(-endPos)"
    printBrInd 3 "with"
    printBrInd 3 "| e -> brs.[0].back_ref.GetDocumentRange())"
    printBrInd 1 "ranges"
    res.ToString()

//prints "tokenToTreeNode" function in parser file. 
//function "tokenToTreeNode" needs in highlihgting after lexical analysis.
let printTokenToTreeNode (indexator : Indexator) = 
    let res  = new System.Text.StringBuilder()

    let inline print (x : 'a) =
        Printf.kprintf (fun s -> res.Append s |> ignore) x

    let inline printBr (x : 'a) =
        Printf.kprintf (fun s -> res.Append(s).Append('\n') |> ignore) x

    let inline printBrInd num (x : 'a) =
        print "%s" (String.replicate (num <<< 2) " ")
        printBr x

    printBrInd 0 "let tokenToTreeNode token = "
    printBrInd 1 "match token with"
            
    for i = indexator.termsStart to indexator.termsEnd do
        let termNode = toClassName <| indexator.indexToTerm i
        printBrInd 1 "| %s data -> " termNode
        printBrInd 2 "let value, temp = data"
        printBrInd 2 "let ranges = calculatePos temp"
        printBrInd 2 "new %sTermNode(value.ToString(), ranges) :> ITreeNode" termNode

    for i = indexator.literalsStart to indexator.literalsEnd do
        let litNode = toClassName <| indexator.indexToLiteral i
        printBrInd 1 "| L_%s data -> " <| indexator.indexToLiteral i
        printBrInd 2 "let value, temp = data"
        printBrInd 2 "let ranges = calculatePos temp"
        printBrInd 2 "new %sLitNode(value.ToString(), ranges) :> ITreeNode" litNode

    res.ToString()

let printItemsGroup nameOfClasses xmlName = 
    let res = new System.Text.StringBuilder()

    let inline print (x : 'a) =
        Printf.kprintf (fun s -> res.Append s |> ignore) x

    let inline printBr (x : 'a) =
        Printf.kprintf (fun s -> res.Append(s).Append('\n') |> ignore) x

    let inline printBrInd num (x : 'a) =
        print "%s" (String.replicate (num <<< 1) " ")
        printBr x
    
    printBrInd 0 "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    printBrInd 0 "<!-- Generate file must be include in .csproj with help follow strings "
    printBrInd 0 "<Import Project=\"ItemsGroup.target\" />"
    printBrInd 0 "<ItemGroup> <Compile Include=\"@(ExternalCompile)\" /></ItemGroup> -->"
    printBrInd 1 "<Project ToolsVersion=\"4.0\" DefaultTargets=\"Build\" xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">"
    printBrInd 1 "<ItemGroup>" 
    printBrInd 2 "<ExternalCompile Include=\"Properties\AssemblyInfo.cs\" />"
    for className in nameOfClasses do
        printBrInd 2 "<ExternalCompile Include=\"%s\" />" className

    printBrInd 2 "<Content Include=\"%s.xml\">" xmlName
    printBrInd 3 "<CopyToOutputDirectory>Always</CopyToOutputDirectory>"
    printBrInd 2 "</Content>"

    printBrInd 1 "</ItemGroup>"
    printBrInd 1 "</Project>"
    res.ToString()

let generate (indexator : Indexator) namespaceName = 
    let folder = System.IO.Path.GetFullPath namespaceName + "\\"
    let langName = namespaceName.Replace ("Highlighting", "")
    let baseClass = langName + baseClassSuffix
    
    let generateFile path text = 
        use out = new System.IO.StreamWriter(path : string)
        out.WriteLine(text : string)
        out.Close()

    let generateXML() = 
        let fileName = folder + baseClass + extension
        let text = printBaseTreeNode namespaceName baseClass langName 
        generateFile fileName text
    
    generateXML()

    let mutable tokensAndLits = []
    let mutable nameOfClasses = []
                
    for i = 0 to indexator.nonTermCount - 1 do
        let name = indexator.indexToNonTerm i
        if not <| name.Contains ("highlight_")
        then 
            nameOfClasses <- name + nonTermSuffix + extension :: nameOfClasses
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
                    
        nameOfClasses <- name + termSuffix + extension :: nameOfClasses
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
                    
        nameOfClasses <- name + literalSuffix + extension :: nameOfClasses
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
        let path = folder + name + ".xml"
        if not <| System.IO.File.Exists (path)
        then 
            let text = printXML name toksAndLits
            generateFile path text
    generateXML namespaceName tokensAndLits
    
    nameOfClasses <- nameOfClasses |> List.rev

    let generateItemGroup() =
        let fileName = folder + "ItemsGroup.target"
        let text = printItemsGroup <| List.rev (baseClass + extension :: nameOfClasses) <| namespaceName
        generateFile fileName text
    
    generateItemGroup()

let printHotspotFile() = 
    let res  = new System.Text.StringBuilder()

    let inline print (x : 'a) =
        Printf.kprintf (fun s -> res.Append s |> ignore) x

    let inline printBr (x : 'a) =
        Printf.kprintf (fun s -> res.Append(s).Append('\n') |> ignore) x

    let inline printBrInd num (x : 'a) =
        print "%s" (String.replicate (num <<< 1) " ")
        printBr x

    printBrInd 0 "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    printBrInd 0 "<Body>"
    printBrInd 0 "<!-- enter name of language. For example, TSQL-->"
    printBrInd 1 "<Language name=\"\">"
    printBrInd 2 "<Hotspot>"
    printBrInd 2 "<!-- Format: \"<Class>.<Method>\". For example, \"Program.Eval\" -->"
    printBrInd 3 "<Fullname></Fullname>"
    printBrInd 3 "<!-- If you call Program.Eval(query, parameters) where variable parameters is parameters of connection, then value ArgumentPosition is 0 (zero-based)-->"
    printBrInd 3 "<ArgumentPosition> 0 </ArgumentPosition>"
    printBrInd 3 "type of result of query: int, string, void etc"
    printBrInd 3 "<ReturnType></ReturnType>"
    printBrInd 2 "</Hotspot>"
    printBrInd 1 "</Language>"
    res.ToString()