module PrintTreeNode

let printTreeNode (nameOfNamespace : string) (nameOfClass : string) = 
    let res  = new System.Text.StringBuilder()

    let inline print (x : 'a) =
        Printf.kprintf (fun s -> res.Append s |> ignore) x

    let inline printBr (x : 'a) =
        Printf.kprintf (fun s -> res.Append(s).Append('\n') |> ignore) x

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
    printBrInd 3 "UserData.PutData(KeyConstant.YcTokName, ycTokName);"
    printBrInd 3 "UserData.PutData(KeyConstant.YcValue, ycValue);"
    printBr ""
    printBrInd 3 "YcHelper.AddYcItem(ycTokName, ycValue);"
    printBrInd 2 "}"
    printBr ""
        
    printBrInd 2 "public %s (string ycTokName, string ycValue, object positions) : this (ycTokName, ycValue)" nameOfClass
    printBrInd 2 "{"
    printBrInd 3 "SetPositions(positions as IEnumerable<DocumentRange>);"
    printBrInd 2 "}"
    printBr ""
    // printing all methods

    printBrInd 2 "private void SetPositions(IEnumerable<DocumentRange> positions)"
    printBrInd 2 "{"
    printBrInd 3 "if (positions == null)"
    printBrInd 4 "return;"
    printBr ""
    printBrInd 3 "var ranges = positions.ToList();"
    printBrInd 3 "UserData.PutData(KeyConstant.Ranges, ranges);"
    printBrInd 2 "}"
    printBr ""

    (*
    printBrInd 2 "public DocumentRange[] GetAllPositions()"
    printBrInd 2 "{"
    printBrInd 3 "return ranges.ToArray();"
    printBrInd 2 "}"
    printBr ""
    *)

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
    printBrInd 3 "ITreeNode needNode = null;"
    printBrInd 3 "for (ITreeNode child = this.FirstChild; child != null; child = child.NextSibling)"
    printBrInd 3 "{"
    printBrInd 4 "var childOffset = child.GetTreeStartOffset();"
    printBrInd 4 "if (!childOffset.IsValid())"
    printBrInd 5 "continue;"
    printBrInd 4 "if (child.GetTreeStartOffset() <= treeTextRange.StartOffset)"
    printBrInd 5 "needNode = child;"
    printBrInd 4 "else"
    printBrInd 5 "break;"
    printBrInd 3 "}"
    printBrInd 3 "//needNode = needNode.PrevSibling;"
    printBr ""
    printBrInd 3 "if (needNode == null || needNode.FirstChild == null) return needNode;"
    printBrInd 3 "else return needNode.FindNodeAt(treeTextRange);"
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
    (*
    printBrInd 2 "public void SetParent(ITreeNode parent)"
    printBrInd 2 "{"
    printBrInd 3 "Parent = parent;"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public void SetFirstChild(ITreeNode firstChild)"
    printBrInd 2 "{"
    printBrInd 3 "FirstChild = firstChild;"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public void SetLastChild(ITreeNode lastChild)"
    printBrInd 2 "{"
    printBrInd 3 "LastChild = lastChild;"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public void SetNextSibling(ITreeNode nextSibling)"
    printBrInd 2 "{"
    printBrInd 3 "NextSibling = nextSibling;"
    printBrInd 2 "}"
    printBr ""

    printBrInd 2 "public void SetPrevSibling(ITreeNode prevSibling)"
    printBrInd 2 "{"
    printBrInd 3 "PrevSibling = prevSibling;"
    printBrInd 2 "}"
    
    printBrInd 2 "public void SetPsiLanguageType(PsiLanguageType languageType)"
    printBrInd 2 "{"
    printBrInd 3 "Language = languageType;"
    printBrInd 2 "}"
    
    printBrInd 2 "public void SetNodeUserData(NodeUserData userData)"
    printBrInd 2 "{"
    printBrInd 3 "UserData = userData;"
    printBrInd 2 "}"

    printBrInd 2 "public void SetPersistentUserData(NodeUserData persistentUserData)"
    printBrInd 2 "{"
    printBrInd 3 "PersistentUserData = persistentUserData;"
    printBrInd 2 "}"
    *)

//    printBrInd 2 "public void Accept(TreeNodeVisitor visitor)"
//    printBrInd 2 "{"
//    printBrInd 3 "visitor.VisitNode(this);"
//    printBrInd 2 "}"

//    printBrInd 2 "public void Accept<TContext>(TreeNodeVisitor<TContext> visitor, TContext context)"
//    printBrInd 2 "{"
//    printBrInd 3 "visitor.VisitSomething(this, context);"
//    printBrInd 2 "}"

//    printBrInd 2 "public TResult Accept<TContext, TResult>(TreeNodeVisitor<TContext, TResult> visitor, TContext context)"
//    printBrInd 2 "{"
//    printBrInd 3 "visitor.VisitSomething(this, context);"
//    printBrInd 3 "return default(TResult);"
//    printBrInd 2 "}"

    printBrInd 1 "}"
    printBrInd 0 "}"
    res.ToString()


let printXML (nameOfNamespace : string) tokens = 
    let res  = new System.Text.StringBuilder()

    let inline print (x : 'a) =
        Printf.kprintf (fun s -> res.Append s |> ignore) x

    let inline printBr (x : 'a) =
        Printf.kprintf (fun s -> res.Append(s).Append('\n') |> ignore) x

    let inline printBrInd num (x : 'a) =
        print "%s" (String.replicate (num <<< 2) " ")
        printBr x

    let availableColors = 
        [
            "ANALYSIS_ERROR_ERRORSTRIPE";
            "ANALYSIS_SUGGESTION_ERRORSTRIPE";
            "ANALYSIS_WARNING_ERRORSTRIPE";
            "CLASS_IS_INHERITED_ATTRIBUTE";
            "CONSTANT_IDENTIFIER_ATTRIBUTE";
            "DEADCODE_ATTRIBUTE";
            "ERROR_ATTRIBUTE";
            "EVENT_IDENTIFIER_ATTRIBUTE";
            "EXTENSION_METHOD_IDENTIFIER_ATTRIBUTE";
            "FIELD_IDENTIFIER_ATTRIBUTE";
            "FORMAT_STRING_ITEM";
            "HIDES_ATTRIBUTE";
            "HINT_ATTRIBUTE";
            "IMPLEMENTS_AND_OVERRIDES_ATTRIBUTE";
            "IMPLEMENTS_AND_HIDES_ATTRIBUTE";
            "IMPLEMENTS_ATTRIBUTE";
            "INTERFACE_IS_IMPLEMENTED_ATTRIBUTE";
            "JAVA_SCRIPT_XML_DOC_TAG";
            "LATE_BOUND_IDENTIFIER_ATTRIBUTE";
            "LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE";
            //"MATCHED_BRACE";
            "MATCHED_FORMAT_STRING_ITEM";
            "METHOD_IDENTIFIER_ATTRIBUTE";
            "MUTABLE_LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE";
            "NAMESPACE_IDENTIFIER_ATTRIBUTE";
            "OPERATOR_IDENTIFIER_ATTRIBUTE";
            //"OUTLINE_BRACE";
            "OVERRIDES_ATTRIBUTE";
            "PARAMETER_IDENTIFIER_ATTRIBUTE";
            "PATH_IDENTIFIER_ATTRIBUTE";
            "PUBLIC_DEADCODE_ATTRIBUTE";
            "RECURSION_ATTRIBUTE";
            "SUGGESTION_ATTRIBUTE";
            "TODOITEM_ATTRIBUTE";
            "TODOITEM_ERRORSTRIPE_ATTRIBUTE";
            "TYPE_CLASS_ATTRIBUTE";
            "TYPE_DELEGATE_ATTRIBUTE";
            "TYPE_ENUM_ATTRIBUTE";
            "TYPE_INTERFACE_ATTRIBUTE";
            "TYPE_PARAMETER_ATTRIBUTE";
            "TYPE_STATIC_CLASS_ATTRIBUTE";
            "TYPE_STRUCT_ATTRIBUTE";
            "UNMATCHED_BRACE";
            "UNRESOLVED_ERROR_ATTRIBUTE";
            "WARNING_ATTRIBUTE"; 
        ]

    printBrInd 0 "<?xml version=\"1.0\"?>"
    printBrInd 0 "<!-- Available color definitions:"
    for color in availableColors do
        printBrInd 1 "%s" color
    printBrInd 0 "-->"

    printBrInd 0 "<SyntaxDefinition name=\"%s\">" nameOfNamespace
    printBrInd 1 "<Tokens color=\"CONSTANT_IDENTIFIER_ATTRIBUTE\">"

    for tok in tokens do
        printBrInd 2 "<Token> %s </Token>" tok

    printBrInd 1 "</Tokens>"

    
    printBrInd 0 "<!-- Dynamic highlighting:"
    printBrInd 1 "<Matched>"
    let pair = [ "(", ")"; "[", "]"; "{", "}"]

    for left, right in pair do
        printBrInd 2 "<Pair>"
        printBrInd 3 "<Left> %s </Left>" left
        printBrInd 3 "<Right> %s </Right>" right
        printBrInd 2 "</Pair>"
    printBrInd 1 "</Matched>"
    printBrInd 0 "-->"

    printBrInd 0 "</SyntaxDefinition>"
    res.ToString()