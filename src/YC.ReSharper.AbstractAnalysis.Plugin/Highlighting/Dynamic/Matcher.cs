using System;
using System.Collections.Generic;
using Highlighting.Core;
using JetBrains.DataFlow;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon.CaretDependentFeatures;
using JetBrains.ReSharper.Feature.Services.Bulbs;
using JetBrains.ReSharper.Feature.Services.ContextHighlighters;
using JetBrains.ReSharper.Feature.Services.CSharp.Bulbs;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.CSharp.Parsing;
using JetBrains.ReSharper.Psi.Parsing;
using JetBrains.ReSharper.Psi.Tree;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting.Dynamic
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
            [ContextKey(typeof(CSharpContextActionDataProvider.ContextKey))] IContextActionDataProvider dataProvider,
            InvisibleBraceHintManager invisibleBraceHintManager,
            MatchingBraceSuggester matchingBraceSuggester)
        {
            return new MatchingBraceContextHighlighter(dataProvider).ProcessDataContextImpl(lifetime, dataProvider, invisibleBraceHintManager, matchingBraceSuggester);
        }

        // We have left brace. We'll find all right braces.
        // '[caret]LBRACE'
        protected override void TryHighlightToRight(MatchingHighlightingsConsumer consumer, ITokenNode selectedToken, TreeOffset treeOffset)
        {
            if (selectedToken.GetTokenType() != CSharpTokenType.STRING_LITERAL)
                return;

            if (MatcherHelper.YcProcessor == null || MatcherHelper.NodeCover.Count == 0)
                return;

            DocumentRange lBraceRange = myProvider.DocumentCaret.ExtendRight(1);

            string lBrotherText = lBraceRange.GetText();
            //if (!MatcherHelper.AllMatchingValues.Contains(lBrotherText)) return;

            string lang = GetLanguageFromRange(lBraceRange);
            if (string.IsNullOrEmpty(lang))
                return;

            string rBrother = LanguageHelper.GetBrother(lang, lBrotherText, Brother.Right);
            if (string.IsNullOrEmpty(rBrother))
                return;

            List<ITreeNode> forest = MatcherHelper.YcProcessor.GetForestWithToken(lang, lBraceRange);

            var offset = new TreeOffset(lBraceRange.TextRange.StartOffset);
            var lBraceTextRange = new TreeTextRange(offset, 1);

            var rightRanges = new List<DocumentRange>();

            foreach (ITreeNode tree in forest)
            {
                var lbraceNode = tree.FindNodeAt(lBraceTextRange);
                var rBraceNode = lbraceNode.NextSibling;
                while (rBraceNode != null
                    && rBraceNode.UserData.GetData(KeyConstant.YcTokName) != rBrother)
                {
                    rBraceNode = rBraceNode.NextSibling;
                }
                if (rBraceNode != null)
                    rightRanges.Add(rBraceNode.GetNavigationRange());
            }

            foreach (DocumentRange range in rightRanges)
            {
                MatchingBracesContextHighlightersUtil.ConsumeMatchingBracesHighlighting(consumer, lBraceRange, range);
            }
        }

        // We have right brace. We'll find all left braces.
        // 'RBRACE[caret]'
        protected override void TryHighlightToLeft(MatchingHighlightingsConsumer consumer, ITokenNode selectedToken, TreeOffset treeOffset)
        {
            if (selectedToken.GetTokenType() != CSharpTokenType.STRING_LITERAL)
                return;

            if (MatcherHelper.YcProcessor == null || MatcherHelper.NodeCover.Count == 0)
                return;

            DocumentRange rBraceRange = myProvider.DocumentCaret.ExtendLeft(1);

            string rBrotherText = rBraceRange.GetText();
            //if (!MatcherHelper.AllMatchingValues.Contains(rBrotherText)) return;

            string lang = GetLanguageFromRange(rBraceRange);
            if (string.IsNullOrEmpty(lang))
                return;
            string lbrother = LanguageHelper.GetBrother(lang, rBrotherText, Brother.Left);

            if (string.IsNullOrEmpty(lbrother))
                return;

            List<ITreeNode> forest = MatcherHelper.YcProcessor.GetForestWithToken(lang, rBraceRange);

            var offset = new TreeOffset(rBraceRange.TextRange.StartOffset);
            var lBraceTextRange = new TreeTextRange(offset, 1);

            var leftRanges = new List<DocumentRange>();

            foreach (ITreeNode tree in forest)
            {
                var rBraceNode = tree.FindNodeAt(lBraceTextRange);
                var lbraceNode = rBraceNode.PrevSibling;
                while (lbraceNode != null
                    && lbraceNode.UserData.GetData(KeyConstant.YcTokName) != lbrother)
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
        }

        private string GetLanguageFromRange(DocumentRange needRange)
        {
            var nodes = new List<ITreeNode>(MatcherHelper.NodeCover);

            foreach (var treeNode in nodes)
            {
                List<DocumentRange> nodeRange = treeNode.UserData.GetData(KeyConstant.Ranges);

                if (nodeRange == null) continue;
                
                foreach (var rng in nodeRange)
                {
                    if (needRange.ContainedIn(rng))
                        return treeNode.UserData.GetData(KeyConstant.YcLanguage);
                }
            }
            return null;
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
    }
}
