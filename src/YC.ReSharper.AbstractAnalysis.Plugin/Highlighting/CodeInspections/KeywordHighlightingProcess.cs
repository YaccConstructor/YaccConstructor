using Highlighting.CodeInspections;
using Highlighting.Psi.CodeInspections.Highlightings;
using JetBrains.Annotations;
using JetBrains.Application.Settings;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi.Tree;

namespace Highlighting.Psi.CodeInspections
{
    public class KeywordHighlightingProcess  : MyIncrementalDaemonStageProcessBase
    {
        public KeywordHighlightingProcess(IDaemonProcess process, IContextBoundSettingsStore settingsStore)
            : base(process, settingsStore)
        {
        }

        public override void VisitNode(ITreeNode node, IHighlightingConsumer consumer)
        {
            AddHighlighting(consumer, node);
        }

        private void AddHighlighting([NotNull] IHighlightingConsumer consumer, [NotNull] ITreeNode expression)
        {
            consumer.AddHighlighting(new MyKeywordHighlighting(expression), File);
        }

        private void AddHighlighting([NotNull] IHighlightingConsumer consumer, IHighlighting highlighting)
        {
            consumer.AddHighlighting(highlighting, File);
        }
    }
}
