using System;
using System.Collections.Generic;

using JetBrains.Annotations;
using JetBrains.DataFlow;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon.CaretDependentFeatures;
using JetBrains.ReSharper.Feature.Services.ContextActions;
using JetBrains.ReSharper.Feature.Services.Contexts;
using JetBrains.ReSharper.Feature.Services.CSharp.Bulbs;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.CSharp.Parsing;
using JetBrains.ReSharper.Psi.Parsing;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Util;
using YC.SDK.ReSharper;

namespace ReSharperExtension.Highlighting.Dynamic
{
    [ContainsContextConsumer]
    public class MatchingBraceContextHighlighter : MatchingBraceContextHighlighterBase
    {
        private IContextActionDataProvider myProvider;

        public MatchingBraceContextHighlighter(IContextActionDataProvider provider)
        {
            myProvider = provider;
        }

        [AsyncContextConsumer]
        public static Action ProcessDataContext(
            Lifetime lifetime, 
            [NotNull, ContextKey(typeof (CSharpContextActionDataProvider.ContextKey))] IContextActionDataProvider dataProvider,
            InvisibleBraceHintManager invisibleBraceHintManager, 
            MatchingBraceSuggester matchingBraceSuggester, 
            HighlightingProlongedLifetime prolongedLifetime)
        {
            return new MatchingBraceContextHighlighter(dataProvider).ProcessDataContextImpl(lifetime, prolongedLifetime, dataProvider, invisibleBraceHintManager, matchingBraceSuggester);
        }

        // We have left brace. We'll find all right braces.
        // '[caret]LBRACE'
        protected override void TryHighlightToRight(MatchingHighlightingsConsumer consumer, ITokenNode selectedToken, TreeOffset treeOffset)
        {
            if (selectedToken.GetTokenType() != CSharpTokenType.STRING_LITERAL_VERBATIM)
                return;

            if (ExistingTreeNodes.ExistingTrees.Count == 0)
                return;

            DocumentRange lBraceRange = myProvider.DocumentCaret.ExtendRight(1);
            ITreeNode node = GetNodeFromRange(lBraceRange);
            string lang = GetLanguageFromNode(node);

            if (String.IsNullOrEmpty(lang))
                return;

            string lBrother = node.UserData.GetData(Constants.YcTokenName);

            string rBrother = LanguageHelper.GetBrother(lang, lBrother, Brother.Right);
            if (String.IsNullOrEmpty(rBrother))
                return;

            int leftNumber = Int32.Parse(node.UserData.GetData(Constants.YcTokNumber));
            int rightNumber = LanguageHelper.GetNumberFromYcName(lang, rBrother);

            var helper = Helper.ReSharperHelper<DocumentRange, ITreeNode>.Instance;

            IEnumerable<DocumentRange> ranges = helper.GetPairedRanges(lang, leftNumber, rightNumber, lBraceRange, true);
            
            foreach (DocumentRange range in ranges)
            {
                MatchingBracesContextHighlightersUtil.ConsumeMatchingBracesHighlighting(consumer, lBraceRange, range);
            }
            /*
             * need for measurement
            Console.WriteLine();
            Stopwatch timer = new Stopwatch();
            timer.Start();
            List<ITreeNode> forest = Helper.ReSharperHelper<DocumentRange, ITreeNode>.Instance.GetForestWithToken(lang, lBraceRange);

            var lBraceTextRange = new TreeTextRange(treeOffset, 1);

            var rightRanges = new List<DocumentRange>();

            foreach (ITreeNode tree in forest)
            {
                var lbraceNode = tree.FindNodeAt(lBraceTextRange);
                //if (lbraceNode == null)
                ////in general, this should not be. But while such a situation occurs
                //    continue;
                var rBraceNode = lbraceNode.NextSibling;
                while (rBraceNode != null
                    && rBraceNode.UserData.GetData(Constants.YcTokenName) != rBrother)
                {
                    var text = rBraceNode.UserData.GetData(Constants.YcTokenName);
                    if (string.IsNullOrEmpty(text))
                        Console.WriteLine();
                    rBraceNode = rBraceNode.NextSibling;
                }
                if (rBraceNode != null)
                    rightRanges.Add(rBraceNode.GetNavigationRange());
            }
            timer.Stop();
            measure.Add(timer.Elapsed);
            if (measure.Count == 10)
            {
                using (var str = new StreamWriter(String.Format(newName, measure.Count)))
                {
                    foreach (TimeSpan span in measure)
                    {
                        str.WriteLine(span);
                    }
                }
            }
            
            foreach (DocumentRange range in rightRanges)
            {
                MatchingBracesContextHighlightersUtil.ConsumeMatchingBracesHighlighting(consumer, lBraceRange, range);
            }
            */
        }

        // We have right brace. We'll find all left braces.
        // 'RBRACE[caret]'
        protected override void TryHighlightToLeft(MatchingHighlightingsConsumer consumer, ITokenNode selectedToken, TreeOffset treeOffset)
        {
            if (selectedToken.GetTokenType() != CSharpTokenType.STRING_LITERAL_VERBATIM)
                return;

            if (ExistingTreeNodes.ExistingTrees.Count == 0)
                return;

            DocumentRange rBraceRange = myProvider.DocumentCaret.ExtendLeft(1);

            ITreeNode node = GetNodeFromRange(rBraceRange);
            string lang = GetLanguageFromNode(node);

            if (String.IsNullOrEmpty(lang))
                return;

            string rBrother = node.UserData.GetData(Constants.YcTokenName);

            string lbrother = LanguageHelper.GetBrother(lang, rBrother, Brother.Left);

            if (String.IsNullOrEmpty(lbrother))
                return;

            int leftNumber = LanguageHelper.GetNumberFromYcName(lang, lbrother);
            int rightNumber = Int32.Parse(node.UserData.GetData(Constants.YcTokNumber));

            var helper = Helper.ReSharperHelper<DocumentRange, ITreeNode>.Instance;

            IEnumerable<DocumentRange> ranges = helper.GetPairedRanges(lang, leftNumber, rightNumber, rBraceRange, false);
            foreach (DocumentRange range in ranges)
            {
                MatchingBracesContextHighlightersUtil.ConsumeMatchingBracesHighlighting(consumer, range, rBraceRange);
            }

            /*
             * need for measurement
            List<ITreeNode> forest = Helper.ReSharperHelper<DocumentRange, ITreeNode>.Instance.GetForestWithToken(lang, rBraceRange);

            var lBraceTextRange = new TreeTextRange(treeOffset.Shift(-1), 1);

            var leftRanges = new List<DocumentRange>();

            foreach (ITreeNode tree in forest)
            {
                var rBraceNode = tree.FindNodeAt(lBraceTextRange);
                //if (rBraceNode == null)
                //    //in general, this should not be. But while such a situation occurs
                //    continue;

                var lbraceNode = rBraceNode.PrevSibling;
                while (lbraceNode != null
                    && lbraceNode.UserData.GetData(Constants.YcTokenName) != lbrother)
                {
                    lbraceNode = lbraceNode.PrevSibling;
                }
                if (lbraceNode != null)
                    leftRanges.Add(lbraceNode.GetNavigationRange());
            }

            foreach (DocumentRange range in leftRanges)
            {
                MatchingBracesContextHighlightersUtil.ConsumeMatchingBracesHighlighting(consumer, range, rBraceRange);
            }
            */
        }

        private ITreeNode GetNodeFromRange(DocumentRange needRange)
        {
            IDocument doc = needRange.Document;

            var treeList = new List<ITreeNode>(ExistingTreeNodes.GetTreeNodes(doc));
            foreach (ITreeNode tree in treeList)
            {
                List<DocumentRange> treeRanges = tree.UserData.GetData(Constants.Ranges);

                if (treeRanges == null)
                    continue;

                foreach (DocumentRange range in treeRanges)
                {
                    if (needRange.ContainedIn(range))
                        return tree.FindNodeAt(GetTreeTextRange(needRange.TextRange));
                }
            }

            return null;
        }

        private string GetLanguageFromNode(ITreeNode node)
        {
            return node == null ? null : node.UserData.GetData(Constants.YcLanguage);
        }

        //Method doesn't call now
        protected override bool IsLeftBracket(TokenNodeType tokenType)
        {
            return false;
        }

        //Method doesn't call now
        protected override bool IsRightBracket(TokenNodeType tokenType)
        {
            return false;
        }

        //Method doesn't call now
        protected override bool Match(TokenNodeType token1, TokenNodeType token2)
        {
            return false;
        }

        private static TreeTextRange GetTreeTextRange(TextRange textRange)
        {
            return new TreeTextRange(new TreeOffset(textRange.StartOffset), new TreeOffset(textRange.EndOffset));
        }
    }
}
