using System;
using System.Collections.Generic;
using System.Linq;
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
using CalcHighlighting;
using YC.ReSharper.AbstractAnalysis.Plugin.Highlighting;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Dynamic
{
    [ContainsContextConsumer]
    public class CalcMatchingBraceContextHighlighter : MatchingBraceContextHighlighterBase
    {
        private IContextActionDataProvider myProvider;
        public CalcMatchingBraceContextHighlighter(IContextActionDataProvider provider)
        {
            myProvider = provider;
        }
        
        [AsyncContextConsumer]
        public static Action ProcessDataContext(
            Lifetime lifetime, 
            [ContextKey(typeof(CSharpContextActionDataProvider.ContextKey))] IContextActionDataProvider dataProvider, 
            InvisibleBraceHintManager invisibleBraceHintManager, 
            MatchingBraceSuggester matchingBraceSuggester/*,
            [ContextKey(typeof(MyActionProvider.ContextKey))] IContextActionDataProvider myProvider*/)
        {
            return new CalcMatchingBraceContextHighlighter(dataProvider).ProcessDataContextImpl(lifetime, dataProvider, invisibleBraceHintManager, matchingBraceSuggester);
        }

        // We have left brace. We'll find all right braces.
        // '[caret]LBRACE'
        protected override void TryHighlightToLeft(MatchingHighlightingsConsumer consumer, ITokenNode selectedToken, TreeOffset treeOffset)
        {
            if (selectedToken.GetTokenType() != CSharpTokenType.STRING_LITERAL)
                return;

            if (MatcherHelper.YcProcessor == null)
                return;
            
            DocumentRange lBraceRange = myProvider.DocumentCaret.ExtendRight(1);
            
            var lBrotherText = lBraceRange.GetText();

            var rBrotherText = MatcherHelper.GetMatch(lBrotherText);
            if (string.IsNullOrEmpty(rBrotherText))
                return;

            List<ITreeNode> forest = MatcherHelper.YcProcessor.GetForestWithToken(lBraceRange);

            var offset = new TreeOffset(lBraceRange.TextRange.StartOffset);
            var lBraceTextRange = new TreeTextRange(offset, 1);

            var rightRanges = new List<DocumentRange>();
            
            foreach (ITreeNode tree in forest)
            {
                var lbraceNode = tree.FindNodeAt(lBraceTextRange);
                var rBraceNode = lbraceNode.NextSibling;
                while (rBraceNode != null 
                    && rBraceNode.UserData.GetData(KeyConstant.YcValue) != rBrotherText)
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
        protected override void TryHighlightToRight(MatchingHighlightingsConsumer consumer, ITokenNode selectedToken, TreeOffset treeOffset)
        {
            if (selectedToken.GetTokenType() != CSharpTokenType.STRING_LITERAL)
                return;

            if (MatcherHelper.YcProcessor == null)
                return;
            
            //if (myProvider.DocumentCaret.TextRange.StartOffset == 0)
            //    return;
            DocumentRange rBraceRange = myProvider.DocumentCaret.ExtendLeft(1);

            var rBrotherText = rBraceRange.GetText();
            var lBrotherText = MatcherHelper.GetMatch(rBrotherText);
            
            if (string.IsNullOrEmpty(lBrotherText))
                return;

            List<ITreeNode> forest = MatcherHelper.YcProcessor.GetForestWithToken(rBraceRange);

            var offset = new TreeOffset(rBraceRange.TextRange.StartOffset);
            var lBraceTextRange = new TreeTextRange(offset, 1);

            var leftRanges = new List<DocumentRange>();
            
            foreach (ITreeNode tree in forest)
            {
                var rBraceNode = tree.FindNodeAt(lBraceTextRange);
                var lbraceNode = rBraceNode.PrevSibling;
                while (lbraceNode != null
                    && lbraceNode.UserData.GetData(KeyConstant.YcValue) != lBrotherText)
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
