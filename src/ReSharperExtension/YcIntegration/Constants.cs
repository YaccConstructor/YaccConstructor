using System.Collections.Generic;

using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Util;

namespace ReSharperExtension.YcIntegration
{
    public static class Constants
    {
        public static Key<ITreeNode> Parent = new Key<ITreeNode>("Parent");
        public static Key<ITreeNode> PrevSibling = new Key<ITreeNode>("PrevSibling");
        public static Key<ITreeNode> NextSibling = new Key<ITreeNode>("NextSibling");
        public static Key<ITreeNode> FirstChild = new Key<ITreeNode>("FirstChild");
        public static Key<ITreeNode> LastChild = new Key<ITreeNode>("LastChild");
        public static Key<NodeType> NodeType = new Key<NodeType>("NodeType");
        public static Key<PsiLanguageType> Language = new Key<PsiLanguageType>("PsiLanguageType");

        public static Key<string> YcTokenName = new Key<string>("ycTokenName");
        public static Key<string> YcLanguage = new Key<string>("ycLanguage");
        public static Key<string> YcTokNumber = new Key<string>("ycTokNumber");
        public static Key<List<DocumentRange>> Ranges = new Key<List<DocumentRange>>("ranges");
        public static Key<IDocument> Document = new Key<IDocument>("document");
    }
}
