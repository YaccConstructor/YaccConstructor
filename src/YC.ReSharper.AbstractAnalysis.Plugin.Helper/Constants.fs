module Constants

open JetBrains.DocumentModel
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.ExtensionsAPI.Tree

let Parent = new JetBrains.Util.Key<ITreeNode>("Parent");
let PrevSibling = new JetBrains.Util.Key<ITreeNode>("PrevSibling");
let NextSibling = new JetBrains.Util.Key<ITreeNode>("NextSibling");
let FirstChild = new JetBrains.Util.Key<ITreeNode>("FirstChild");
let LastChild = new JetBrains.Util.Key<ITreeNode>("LastChild");
let NodeType = new JetBrains.Util.Key<NodeType>("NodeType");
let Language = new JetBrains.Util.Key<PsiLanguageType>("PsiLanguageType");


let YcTokenName = new JetBrains.Util.Key<string>("ycTokenName")
let YcLanguage = new JetBrains.Util.Key<string>("ycLanguage")
let YcTokNumber = new JetBrains.Util.Key<string>("ycTokNumber")
let Ranges = new JetBrains.Util.Key<ResizeArray<DocumentRange>>("ranges")
let Document = new JetBrains.Util.Key<IDocument>("document")