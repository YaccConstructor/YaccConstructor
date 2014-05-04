using System.Collections.Generic;
using JetBrains.Annotations;
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

        public static NodeUserData GetNodeUserData([NotNull] ITreeNode treeNode)
        {
            return nodeUserDataHolder.GetNodeUserData(treeNode);
        }

        public static NodeUserData GetNodePersistentUserData([NotNull] ITreeNode treeNode)
        {
            return nodeUserDataHolder.GetNodePersistentUserData(treeNode);
        }
    }

    public static class KeyConstant
    {
        /// <summary>
        /// UserData keys
        /// </summary>
        public static readonly Key<string> YcTokName = new Key<string>("ycTokName");
        public static readonly Key<string> YcValue = new Key<string>("ycValue");
        public static readonly Key<List<DocumentRange>> Ranges = new Key<List<DocumentRange>>("ranges");
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
