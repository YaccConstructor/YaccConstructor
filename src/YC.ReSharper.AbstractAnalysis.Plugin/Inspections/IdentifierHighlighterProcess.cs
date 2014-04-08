using JetBrains.Application.Settings;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi.Tree;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Inspections
{
    internal class IdentifierHighlighterProcess : MyIncrementalDaemonStageProcessBase
    {
        public IdentifierHighlighterProcess(IDaemonProcess daemonProcess, IContextBoundSettingsStore settingsStore)
            : base(daemonProcess, settingsStore)
        {
        }

        public override void VisitSomething(ITreeNode node, IHighlightingConsumer consumer)
        {
            DocumentRange colorConstantRange = node.GetNavigationRange();

            AddHighLighting(colorConstantRange, node, consumer, new MyHighlighter(node));
        }

        private void AddHighLighting(DocumentRange range, ITreeNode element, IHighlightingConsumer consumer, IHighlighting highlighting)
        {
            //var myRange = range.SetStartTo(15);
            //myRange = range.SetEndTo(20);
            var myRange = range;
            var info = new HighlightingInfo(myRange, highlighting, new Severity?());
            IFile file = this.File;

            if (file != null)
            {
                consumer.Highlightings.Add(info);
                //consumer.AddHighlighting(info.Highlighting, file);
            }
        }
    }
}
