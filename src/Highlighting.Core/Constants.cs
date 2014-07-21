using System.Collections.Generic;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.ReSharper.Psi.Impl;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Util;

namespace Highlighting.Core
{
    public static class DataHelper
    {
        private static NodeUserDataHolder nodeUserDataHolder = new NodeUserDataHolder();

        public static NodeUserData GetNodeUserData(ITreeNode treeNode)
        {
            return nodeUserDataHolder.GetNodeUserData(treeNode);
        }

        public static NodeUserData GetNodePersistentUserData(ITreeNode treeNode)
        {
            return nodeUserDataHolder.GetNodePersistentUserData(treeNode);
        }
    }

    /// <summary>
    /// UserData keys
    /// </summary>
    public static class KeyConstant
    {
        public static readonly Key<string> YcTokName = new Key<string>("ycTokName");
        public static readonly Key<string> YcValue = new Key<string>("ycValue");
        public static readonly Key<List<DocumentRange>> Ranges = new Key<List<DocumentRange>>("ranges");
        public static readonly Key<string> YcLanguage = new Key<string>("ycLanguage");
    }

    public static class PropertyConstant
    {
        /// <summary>
        /// PersistentUserData keys
        /// </summary>
        public static readonly Key<ITreeNode> Parent = new Key<ITreeNode>("Parent");
        public static readonly Key<ITreeNode> PrevSibling = new Key<ITreeNode>("PrevSibling");
        public static readonly Key<ITreeNode> NextSibling = new Key<ITreeNode>("NextSibling");
        public static readonly Key<ITreeNode> FirstChild = new Key<ITreeNode>("FirstChild");
        public static readonly Key<ITreeNode> LastChild = new Key<ITreeNode>("LastChild");
        public static readonly Key<NodeType> NodeType = new Key<NodeType>("NodeType");
        public static readonly Key<PsiLanguageType> Language = new Key<PsiLanguageType>("PsiLanguageType");
    }
}
