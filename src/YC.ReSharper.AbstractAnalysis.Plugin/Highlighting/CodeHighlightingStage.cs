using Highlighting;
using Highlighting.Core;
using JetBrains.Application.Settings;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.CSharp.Stages;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using SmartResolverProcess = YC.ReSharper.AbstractAnalysis.Plugin.Highlighting.CodeInspections.SmartResolverProcess;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    [DaemonStage(StagesBefore = new[] { typeof(LanguageSpecificDaemonStage) })]
    public class CodeHighlightingStage : CSharpDaemonStageBase /*MyDaemonStageBase//*/
    {
        /// <summary>
        /// Inject solution or shell level components here.
        /// </summary>
        public CodeHighlightingStage()
        {
        }

        protected override IDaemonStageProcess CreateProcess(IDaemonProcess process, IContextBoundSettingsStore settings, DaemonProcessKind processKind, ICSharpFile file)
        {
            //return new IdentifierHighlighterProcess(process, settings);
            var treeNode = new AbstractTreeNode("myTreeNode");
            treeNode.SetDocument(process.Document);
            treeNode.DocumentRangeSetEndTo(100);
            treeNode.DocumentRangeSetStartTo(5);
            TreeNodeHolder.TreeNode = treeNode;
            return new SmartResolverProcess(process);
        }

        //public /*override*/ IEnumerable<IDaemonStageProcess> CreateProcess(IDaemonProcess process, IContextBoundSettingsStore settings, DaemonProcessKind processKind)
        //{
        //    var treeNode = new AbstractTreeNode("myTreeNode");
        //    treeNode.SetDocument(process.Document);
        //    treeNode.DocumentRangeSetEndTo(100);
        //    treeNode.DocumentRangeSetStartTo(5);
        //    TreeNodeHolder.TreeNode = treeNode;
        //    return new List<IDaemonStageProcess> { new SmartResolverProcess(process)};
        //}
    }
}
