using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Utilities;
using System.ComponentModel.Composition;


namespace MyCompany.VSYard.Classifiers
{
    [Export(typeof(ITaggerProvider))]
    [TagType(typeof(IOutliningRegionTag))]
    [ContentType("yard")]
    internal sealed class Parser : ITaggerProvider
    {
        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            //create a single tagger for each buffer.
            Func<ITagger<T>> sc = delegate() { return new OutliningTagger(buffer) as ITagger<T>; };
            return buffer.Properties.GetOrCreateSingletonProperty<ITagger<T>>(sc);
        }
    }

    internal sealed class OutliningTagger : ITagger<IOutliningRegionTag>
    {
        string ellipsis = "...";    //the characters that are displayed when the region is collapsed
        ITextBuffer buffer;
        ITextSnapshot snapshot;
        List<Region> regions;

        public OutliningTagger(ITextBuffer buffer)
        {
            this.buffer = buffer;
            this.snapshot = buffer.CurrentSnapshot;
            this.regions = new List<Region>();
            this.ReParse();
            this.buffer.Changed += BufferChanged;
        }

        public IEnumerable<ITagSpan<IOutliningRegionTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            if (spans.Count == 0)
                yield break;
            List<Region> currentRegions = this.regions;
            ITextSnapshot currentSnapshot = spans[0].Snapshot;
            SnapshotSpan entire = new SnapshotSpan(spans[0].Start, spans[spans.Count - 1].End).TranslateTo(currentSnapshot, SpanTrackingMode.EdgeExclusive);
            int startLineNumber = entire.Start.GetContainingLine().LineNumber;
            int endLineNumber = entire.End.GetContainingLine().LineNumber;
            foreach (var region in currentRegions)
            {
                if (region.StartLine <= endLineNumber &&
                    region.EndLine >= startLineNumber)
                {
                    var startLine = currentSnapshot.GetLineFromLineNumber(region.StartLine);
                    var endLine = currentSnapshot.GetLineFromLineNumber(region.EndLine);

                    var startPosition = startLine.Start.Position + region.StartOffset;
                    var length = (endLine.Start.Position + region.EndOffset) - startPosition;

                    //the region starts at the beginning of the "[", and goes until the *end* of the line that contains the "]".
                    yield return new TagSpan<IOutliningRegionTag>(
                        new SnapshotSpan(currentSnapshot,
                            startLine.Start.Position + region.StartOffset, length),
                        new OutliningRegionTag(false, false, ellipsis, String.Empty));
                }
            }
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

        void BufferChanged(object sender, TextContentChangedEventArgs e)
        {
            // If this isn't the most up-to-date version of the buffer, then ignore it for now (we'll eventually get another change event).
            if (e.After != buffer.CurrentSnapshot)
                return;
            this.ReParse();
        }

        void ReParse()
        {
            ITextSnapshot newSnapshot = buffer.CurrentSnapshot;
            List<Region> newRegions = new List<Region>();
            var text = newSnapshot.GetText();
            var lines = newSnapshot.GetText().Count(x => x == '\n');
            #region old stuff
            //LuaLanguage.Grammar grammar = new LuaLanguage.Grammar();
            //Irony.Parsing.Parser parser = new Irony.Parsing.Parser(grammar);
            //var tree = parser.Parse(newSnapshot.GetText());
            //if (tree.Root != null)
            //    FindHiddenRegions(newSnapshot, tree.Root, ref newRegions);


            //determine the changed span, and send a changed event with the new spans
            #endregion
            List<Span> oldSpans =
                new List<Span>(this.regions.Select(r => AsSnapshotSpan(r, this.snapshot)
                    .TranslateTo(newSnapshot, SpanTrackingMode.EdgeExclusive)
                    .Span));
            List<Span> newSpans =
                    new List<Span>(newRegions.Select(r => AsSnapshotSpan(r, newSnapshot).Span));

            NormalizedSpanCollection oldSpanCollection = new NormalizedSpanCollection(oldSpans);
            NormalizedSpanCollection newSpanCollection = new NormalizedSpanCollection(newSpans);

            //the changed regions are regions that appear in one set or the other, but not both.
            NormalizedSpanCollection removed =
            NormalizedSpanCollection.Difference(oldSpanCollection, newSpanCollection);

            int changeStart = int.MaxValue;
            int changeEnd = -1;

            if (removed.Count > 0)
            {
                changeStart = removed[0].Start;
                changeEnd = removed[removed.Count - 1].End;
            }

            if (newSpans.Count > 0)
            {
                changeStart = Math.Min(changeStart, newSpans[0].Start);
                changeEnd = Math.Max(changeEnd, newSpans[newSpans.Count - 1].End);
            }

            this.snapshot = newSnapshot;
            this.regions = newRegions;

            if (changeStart <= changeEnd)
            {
                ITextSnapshot snap = this.snapshot;
                if (this.TagsChanged != null)
                    this.TagsChanged(this, new SnapshotSpanEventArgs(
                        new SnapshotSpan(this.snapshot, Span.FromBounds(changeStart, changeEnd))));
            }
        }

        #region old stuff
        //static void FindHiddenRegions(ITextSnapshot snapShot, Irony.Parsing.ParseTreeNode root, ref List<Region> regions)
        //{
        //    foreach (var child in root.ChildNodes)
        //    {
        //        Irony.Parsing.Token startToken = null;
        //        Irony.Parsing.Token endToken = null;
        //        int startOffset = 0;

        //        if (child.Term.Name == "table constructor")
        //        {
        //            startToken = child.FirstChild.Token;    // '{' symbol
        //            endToken = child.LastChild.Token;       // '}' symbol
        //        }
        //        else if (child.Term.Name == "function body")
        //        {
        //            startToken = child.ChildNodes[2].Token; // ')' symbol
        //            endToken = child.ChildNodes[4].Token;   // 'end' keyword

        //            //Offset the outline by 1 so we don't hide the ')' symbol.
        //            startOffset = 1;
        //        }

        //        if (startToken != null && endToken != null)
        //        {
        //            if (startToken.Location.Line != endToken.Location.Line)
        //            {
        //                //So the column field of Location isn't always accurate...
        //                //Position and Line are accurate though..
        //                var startLine = snapShot.GetLineFromLineNumber(startToken.Location.Line);
        //                var startLineOffset = startToken.Location.Position - startLine.Start.Position;

        //                var endLine = snapShot.GetLineFromLineNumber(endToken.Location.Line);
        //                var endLineOffset = (endToken.Location.Position + endToken.Length) - endLine.Start.Position;

        //                var region = new Region();
        //                region.StartLine = startToken.Location.Line;
        //                region.StartOffset = startLineOffset + startOffset;

        //                region.EndLine = endToken.Location.Line;
        //                region.EndOffset = endLineOffset;

        //                regions.Add(region);
        //            }
        //        }

        //        FindHiddenRegions(snapShot, child, ref regions);
        //    }
        //}
        #endregion

        static SnapshotSpan AsSnapshotSpan(Region region, ITextSnapshot snapshot)
        {
            var startLine = snapshot.GetLineFromLineNumber(region.StartLine);
            var endLine = (region.StartLine == region.EndLine) ? startLine
                 : snapshot.GetLineFromLineNumber(region.EndLine);
            return new SnapshotSpan(startLine.Start + region.StartOffset, endLine.End);
        }

        class Region
        {
            public int StartLine { get; set; }
            public int StartOffset { get; set; }
            public int EndLine { get; set; }
            public int EndOffset { get; set; }
        }
    }
}