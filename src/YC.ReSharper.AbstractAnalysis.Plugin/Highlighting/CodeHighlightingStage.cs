using System;
using System.Collections.Generic;
using Highlighting.Core;
using JetBrains.Application.Progress;
using JetBrains.Application.Settings;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.CSharp.Stages;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using JetBrains.ReSharper.Psi.Files;

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

	    private void Main(IDaemonProcess daemonProcess)
	    {
			var sourceFile = daemonProcess.SourceFile;
			var file = sourceFile.GetPsiServices().Files.GetDominantPsiFile<CSharpLanguage>(sourceFile) as ICSharpFile;
			if (file == null)
				return;

			// Running visitor against the PSI
			var processor = new YC.ReSharper.AbstractAnalysis.Plugin.Core.Processor(file);
			processor.Process();
			var treeNodeList = processor.TreeNode as IEnumerable<IAbstractTreeNode>;

	        if (treeNodeList == null)
	        {
	            throw new Exception("TreeNode is null!");
	        }
	        var enumerator = treeNodeList.GetEnumerator();

	        if (enumerator.MoveNext())
	        {
                TreeNodeHolder.TreeNode = enumerator.Current;
                TreeNodeHolder.ParseFile("CalcHighlighting.xml");
	        }
                

			// Checking if the daemon is interrupted by user activity
			if (daemonProcess.InterruptFlag)
				throw new ProcessCancelledException();
	    }

        protected override IDaemonStageProcess CreateProcess(IDaemonProcess process, IContextBoundSettingsStore settings, DaemonProcessKind processKind, ICSharpFile file)
        {
			// Getting PSI (AST) for the file being highlighted
	        Main(process);
            return new MySmartResolverProcess(process);
        }

        //public /*override*/ IEnumerable<IDaemonStageProcess> CreateProcess(IDaemonProcess process, IContextBoundSettingsStore settings, DaemonProcessKind processKind)
        //{
        //    var treeNode = new AbstractTreeNode("myTreeNode");
        //    treeNode.SetDocument(process.Document);
        //    treeNode.DocumentRangeSetEndTo(100);
        //    treeNode.DocumentRangeSetStartTo(5);
        //    TreeNodeHolder.TreeNode = treeNode;
        //    return new List<IDaemonStageProcess> { new MySmartResolverProcess(process)};
        //}
    }
}
