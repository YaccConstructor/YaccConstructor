using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;

//It is simple tags 
namespace MyCompany.VSYard.Classifiers
{
    public class TokenTag : ITag
    {
        public TokenTypes type { get; private set; }

        public TokenTag(TokenTypes type)
        {
            this.type = type;
        }
    }

    //internal sealed class TokenTagger : ITagger<TokenTag>
    //{

    //    ITextBuffer _buffer;
    //    IDictionary<string, TokenTypes> _ookTypes;

    //    internal TokenTagger(ITextBuffer buffer)
    //    {
    //        _buffer = buffer;
    //        _ookTypes = new Dictionary<string, TokenTypes>();
    //        _ookTypes["ook!"] = TokenTypes.OokExclaimation;
    //        _ookTypes["ook."] = TokenTypes.OokPeriod;
    //        _ookTypes["ook?"] = TokenTypes.OokQuestion;
    //        //_ookTypes["("] = OokTokenTypes.OBrace;
    //        //_ookTypes[")"] = OokTokenTypes.CBrace;

    //    }
    //    public event EventHandler<SnapshotSpanEventArgs> TagsChanged
    //    {
    //        add { }
    //        remove { }
    //    }

    //    public IEnumerable<ITagSpan<TokenTag>> GetTags(NormalizedSnapshotSpanCollection spans)
    //    {

    //        foreach (SnapshotSpan curSpan in spans)
    //        {
    //            ITextSnapshotLine containingLine = curSpan.Start.GetContainingLine();
    //            int curLoc = containingLine.Start.Position;
    //            string[] tokens = containingLine.GetText().ToLower().Split(' ');

    //            foreach (string ookToken in tokens)
    //            {
    //                if (_ookTypes.ContainsKey(ookToken))
    //                {
    //                    var tokenSpan = new SnapshotSpan(curSpan.Snapshot, new Span(curLoc, ookToken.Length));
    //                    if (tokenSpan.IntersectsWith(curSpan))
    //                        yield return new TagSpan<TokenTag>(tokenSpan,
    //                                                              new TokenTag(_ookTypes[ookToken]));
    //                }

    //                //add an extra char location because of the space
    //                curLoc += ookToken.Length + 1;
    //            }
    //        }

    //    }
    //}

    public enum TokenTypes
    {
        OokExclaimation, OokQuestion, OokPeriod, OBrace, CBrace
    }
}
