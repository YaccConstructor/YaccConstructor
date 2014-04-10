using System.Collections.Generic;
using JetBrains.Application.Settings;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Util;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    public class IdentifierHighlighterProcess : MyIncrementalDaemonStageProcessBase
    {
        private List<TextRange> addedRanges = new List<TextRange>();
     
        public IdentifierHighlighterProcess(IDaemonProcess daemonProcess, IContextBoundSettingsStore settingsStore, DaemonProcessKind processKind)
            : base(daemonProcess, settingsStore, processKind)
        {
        }

        public override void VisitSomething(ITreeNode node, IHighlightingConsumer consumer)
        {
            DocumentRange colorConstantRange = node.GetNavigationRange();

            if (colorConstantRange.Document != null && !addedRanges.Contains(colorConstantRange.TextRange))
            {
                AddHighLighting(colorConstantRange, node, consumer, new MySomethingHighlighting(node));
            }
        }

        private void AddHighLighting(DocumentRange range, ITreeNode element, IHighlightingConsumer consumer, IHighlighting highlighting)
        {
            var info = new HighlightingInfo(range, highlighting, new Severity?());
            IFile file = this.File;

            if (file != null)
            {
                addedRanges.Add(range.TextRange);
                //consumer.Highlightings.Add(info);
                consumer.AddHighlighting(info.Highlighting, file);
            }
        }
    }
}
