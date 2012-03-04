using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Language.Intellisense;
using System.Collections.ObjectModel;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Utilities;

namespace VSYard.AutoCompletion
{
    [Export(typeof(ICompletionSourceProvider))]
    [ContentType("text")]
    [Name("YardCompletion")]
    class YardCompletionSourceProvider : ICompletionSourceProvider
    {
        public ICompletionSource TryCreateCompletionSource(ITextBuffer textBuffer)
        {
            return new VSYardNS.YardCompletionSource(textBuffer);
        }
    }

//    class YardCompletionSource : ICompletionSource
//    {
//        private ITextBuffer _buffer;
//        private bool _disposed = false;

//        public YardCompletionSource(ITextBuffer buffer)
//        {
//            _buffer = buffer;
//        }

//        public void AugmentCompletionSession(ICompletionSession session, IList<CompletionSet> completionSets)
//        {
//            if (_disposed)
//                throw new ObjectDisposedException("YardCompletionSource");

//            List<Completion> completions = new List<Completion>()
//            {
//                new Completion("Oos!"),
//                new Completion("Ook."),
//                new Completion("Ook?")
//            };

//            ITextSnapshot snapshot = _buffer.CurrentSnapshot;
//            var triggerPoint = (SnapshotPoint)session.GetTriggerPoint(snapshot);

//            if (triggerPoint == null)
//                return;

//            var line = triggerPoint.GetContainingLine();
//            SnapshotPoint start = triggerPoint;

//            while (start > line.Start && !char.IsWhiteSpace((start - 1).GetChar()))
//            {
//                start -= 1;
//            }

//            var applicableTo = snapshot.CreateTrackingSpan(new SnapshotSpan(start, triggerPoint), SpanTrackingMode.EdgeInclusive);

//            completionSets.Add(new CompletionSet("All", "All", applicableTo, completions, Enumerable.Empty<Completion>()));
//        }

//        public void Dispose()
//        {
//            _disposed = true;
//        }
//    }
}

