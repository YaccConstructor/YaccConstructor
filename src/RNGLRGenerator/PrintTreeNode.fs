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
    printBrInd 0 "using JetBrains.Text;"
    printBrInd 0 "using Highlighting.Core;"

    printBr "" 

    printBrInd 0 "namespace %s" nameOfNamespace
    printBrInd 0 "{"

    printBrInd 1 "public class %s : IAbstractTreeNode" nameOfClass
    printBrInd 1 "{"
    printBrInd 2 "public ITreeNode Parent { get; private set; }"
    printBrInd 2 "public ITreeNode FirstChild { get; private set; }"
    printBrInd 2 "public ITreeNode LastChild { get; private set; }"
    printBrInd 2 "public ITreeNode NextSibling { get; private set; }"
    printBrInd 2 "public ITreeNode PrevSibling { get; private set; }"
    printBrInd 2 "public NodeType NodeType { get; private set; }"
    printBrInd 2 "public PsiLanguageType Language { get; private set; }"
    printBrInd 2 "public NodeUserData UserData { get; private set; }"
    printBrInd 2 "public NodeUserData PersistentUserData { get; private set; }"
    printBr ""    
    printBrInd 2 "private DocumentRange documentRange = new DocumentRange();"
    printBrInd 2 "private List<DocumentRange> ranges = new List<DocumentRange>();"
    printBrInd 2 "private string text;"
        
    printBr ""    
    printBrInd 2 "public %s (string s)" nameOfClass
    printBrInd 2 "{"
    printBrInd 3 "text = s;"
    printBrInd 2 "}"
    printBr ""
        
    printBrInd 2 "public %s (string s, object positions)" nameOfClass
    printBrInd 2 "{"
    printBrInd 3 "text = s;"
    printBrInd 3 "SetPositions(positions as IEnumerable<DocumentRange>);"
    printBrInd 2 "}"
    printBr ""
    // printing all methods

    printBrInd 2 "public void SetPositions(object obj)"
    printBrInd 2 "{"
    printBrInd 3 "var positions = obj as IEnumerable<DocumentRange>;"
    printBrInd 3 "if (positions != null)"
    printBrInd 3 "{"
    printBrInd 4 "ranges = positions.ToList();"
    printBrInd 4 "documentRange = ranges[0];"
    printBrInd 3 "}"
    printBrInd 2 "}"

    printBrInd 2 "public void SetDocumentRange(DocumentRange range)"
    printBrInd 2 "{"
    printBrInd 3 "documentRange = range;"
    printBrInd 2 "}"

    printBrInd 2 "public void DocumentRangeSetStartTo(int start)"
    printBrInd 2 "{"
    printBrInd 3 "documentRange = documentRange.SetStartTo(start);"
    printBrInd 2 "}"

    printBrInd 2 "public void DocumentRangeSetEndTo(int end)"
    printBrInd 2 "{"
    printBrInd 3 "documentRange = documentRange.SetEndTo(end);"
    printBrInd 2 "}"

    printBrInd 2 "public IPsiServices GetPsiServices()"
    printBrInd 2 "{"
    printBrInd 3 "return default(IPsiServices);"
    printBrInd 2 "}"

    printBrInd 2 "public IPsiModule GetPsiModule()"
    printBrInd 2 "{"
    printBrInd 3 "return this.Parent.GetPsiModule();"
    printBrInd 2 "}"

    printBrInd 2 "public IPsiSourceFile GetSourceFile()"
    printBrInd 2 "{"
    printBrInd 3 "return Parent.GetSourceFile();"
    printBrInd 2 "}"

    printBrInd 2 "public ReferenceCollection GetFirstClassReferences()"
    printBrInd 2 "{"
    printBrInd 3 "return default(ReferenceCollection);"
    printBrInd 2 "}"

    printBrInd 2 "public void ProcessDescendantsForResolve(IRecursiveElementProcessor processor)"
    printBrInd 2 "{"
    printBrInd 3 "return;"
    printBrInd 2 "}"

    printBrInd 2 "public T GetContainingNode<T>(bool returnThis = false) where T : ITreeNode"
    printBrInd 2 "{"
    printBrInd 3"return default(T);"
    printBrInd 2 "}"

    printBrInd 2 "public bool Contains(ITreeNode other)"
    printBrInd 2 "{"
    printBrInd 3 "return true;"
    printBrInd 2 "}"

    printBrInd 2 "public bool IsPhysical()"
    printBrInd 2 "{"
    printBrInd 3 "return true;"
    printBrInd 2 "}"

    printBrInd 2 "public bool IsValid()"
    printBrInd 2 "{"
    printBrInd 3 "return true;"
    printBrInd 2 "}"

    printBrInd 2 "public bool IsStub()"
    printBrInd 2 "{"
    printBrInd 3 "return false;"
    printBrInd 2 "}"

    printBrInd 2 "public bool IsFiltered()"
    printBrInd 2 "{"
    printBrInd 3 "return true;"
    printBrInd 2 "}"

    printBrInd 2 "public DocumentRange GetNavigationRange()"
    printBrInd 2 "{"
    printBrInd 3 "return documentRange;"
    printBrInd 2 "}"

    printBrInd 2 "public TreeOffset GetTreeStartOffset()"
    printBrInd 2 "{"
    printBrInd 3 "return new TreeOffset();"
    printBrInd 2 "}"

    printBrInd 2 "public int GetTextLength()"
    printBrInd 2 "{"
    printBrInd 3 "return text.Length;"
    printBrInd 2 "}"

    printBrInd 2 "public StringBuilder GetText(StringBuilder to)"
    printBrInd 2 "{"
    printBrInd 3 "for (ITreeNode nextSibling = this.FirstChild; nextSibling != null; nextSibling = nextSibling.NextSibling)"
    printBrInd 4 "{"
    printBrInd 5 "nextSibling.GetText(to);"
    printBrInd 4 "}"
    printBrInd 3 "return to;"
    printBrInd 2 "}"

    printBrInd 2 "public IBuffer GetTextAsBuffer()"
    printBrInd 2 "{"
    printBrInd 3 "return new StringBuffer(text);"
    printBrInd 2 "}"

    printBrInd 2 "public string GetText()"
    printBrInd 2 "{"
            //StringBuilder to = (this.MyCachedLength >= 0) ? new StringBuilder(this.myCachedLength) : new StringBuilder();
            //return this.GetText(to).ToString();
    printBrInd 3 "return text;"
    printBrInd 2 "}"

    printBrInd 2 "public ITreeNode FindNodeAt(TreeTextRange treeTextRange)"
    printBrInd 2 "{"
    printBrInd 3 "return null;"
    printBrInd 2 "}"

    printBrInd 2 "public ICollection<ITreeNode> FindNodesAt(TreeOffset treeTextOffset)"
    printBrInd 2 "{"
    printBrInd 3 "return default(ICollection<ITreeNode>);"
    printBrInd 2 "}"

    printBrInd 2 "public ITreeNode FindTokenAt(TreeOffset treeTextOffset)"
    printBrInd 2 "{"
    printBrInd 3 "return null;"
    printBrInd 2 "}"

    printBrInd 2 "public void SetParent(ITreeNode parent)"
    printBrInd 2 "{"
    printBrInd 3 "Parent = parent;"
    printBrInd 2 "}"

    printBrInd 2 "public void SetFirstChild(ITreeNode firstChild)"
    printBrInd 2 "{"
    printBrInd 3 "FirstChild = firstChild;"
    printBrInd 2 "}"

    printBrInd 2 "public void SetLastChild(ITreeNode lastChild)"
    printBrInd 2 "{"
    printBrInd 3 "LastChild = lastChild;"
    printBrInd 2 "}"

    printBrInd 2 "public void SetNextSibling(ITreeNode nextSibling)"
    printBrInd 2 "{"
    printBrInd 3 "NextSibling = nextSibling;"
    printBrInd 2 "}"

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

    printBrInd 2 "public void Accept(TreeNodeVisitor visitor)"
    printBrInd 2 "{"
    printBrInd 3 "visitor.VisitNode(this);"
    printBrInd 2 "}"

    printBrInd 2 "public void Accept<TContext>(TreeNodeVisitor<TContext> visitor, TContext context)"
    printBrInd 2 "{"
    printBrInd 3 "visitor.VisitSomething(this, context);"
    printBrInd 2 "}"

    printBrInd 2 "public TResult Accept<TContext, TResult>(TreeNodeVisitor<TContext, TResult> visitor, TContext context)"
    printBrInd 2 "{"
    printBrInd 3 "visitor.VisitSomething(this, context);"
    printBrInd 3 "return default(TResult);"
    printBrInd 2 "}"

    printBrInd 1 "}"
    printBrInd 0 "}"
    res.ToString()