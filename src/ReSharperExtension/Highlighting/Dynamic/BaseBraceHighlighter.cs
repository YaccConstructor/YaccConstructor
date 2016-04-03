using System;
using System.Collections.Generic;
using System.Linq;

using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon.CaretDependentFeatures;
using JetBrains.ReSharper.Feature.Services.ContextActions;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.Parsing;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Util;

using ReSharperExtension.YcIntegration;

namespace ReSharperExtension.Highlighting.Dynamic
{
    public class BaseBraceHighlighter : MatchingBraceContextHighlighterBase
    {
        private readonly IContextActionDataProvider myProvider;
        protected ITokenNodeType stringLiteral;

        private readonly ReSharperHelper<DocumentRange, ITreeNode> helper = ReSharperHelper<DocumentRange, ITreeNode>.Instance;

        protected BaseBraceHighlighter(IContextActionDataProvider provider)
        {
            myProvider = provider;
        }

        private bool IsStringLiteral(ITokenNode token)
        {
            return token.GetTokenType() == stringLiteral;
        }

        #region MatchingBraceContextHighlighterBase members
        // We have left brace. We'll find all right braces.
        // '[caret]LBRACE'
        protected override void TryHighlightToRight(MatchingHighlightingsConsumer consumer, ITokenNode selectedToken, TreeOffset treeOffset)
        {
            if (!IsStringLiteral(selectedToken))
                return;

            if (ExistingRanges.DocumentToRange.Count == 0)
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

            int leftNumber = Convert.ToInt32(node.UserData.GetData(Constants.YcTokNumber));
            int rightNumber = LanguageHelper.GetNumberFromYcName(lang, rBrother);

            IEnumerable<DocumentRange> ranges = helper.GetPairedRanges(lang, leftNumber, rightNumber, lBraceRange, true);

            foreach (DocumentRange range in ranges)
            {
                MatchingBracesContextHighlightersUtil.ConsumeMatchingBracesHighlighting(consumer, lBraceRange, range);
            }
        }

        // We have right brace. We'll find all left braces.
        // 'RBRACE[caret]'
        protected override void TryHighlightToLeft(MatchingHighlightingsConsumer consumer, ITokenNode selectedToken, TreeOffset treeOffset)
        {
            if (!IsStringLiteral(selectedToken))
                return;

            if (ExistingRanges.DocumentToRange.Count == 0)
                return;

            DocumentRange rBraceRange = myProvider.DocumentCaret.ExtendLeft(1);
            ITreeNode node = GetNodeFromRange(rBraceRange);
            string lang = GetLanguageFromNode(node);

            if (String.IsNullOrEmpty(lang))
                return;

            string rBrother = node.UserData.GetData(Constants.YcTokenName);

            string lBrother = LanguageHelper.GetBrother(lang, rBrother, Brother.Left);
            if (String.IsNullOrEmpty(lBrother))
                return;

            int rightNumber = Convert.ToInt32(node.UserData.GetData(Constants.YcTokNumber));
            int leftNumber = LanguageHelper.GetNumberFromYcName(lang, lBrother);

            IEnumerable<DocumentRange> ranges = helper.GetPairedRanges(lang, leftNumber, rightNumber, rBraceRange, false);

            foreach (DocumentRange range in ranges)
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

#endregion

        private ITreeNode GetNodeFromRange(DocumentRange needRange)
        {
            IDocument doc = needRange.Document;

            var treeList = new List<ITreeNode>(ExistingRanges.GetTreeNodes(doc));
            foreach (ITreeNode tree in treeList)
            {
                List<DocumentRange> treeRanges = tree.UserData.GetData(Constants.Ranges);

                if (treeRanges == null)
                    continue;

                if (treeRanges.Any(range => needRange.ContainedIn(range)))
                    return tree.FindNodeAt(GetTreeTextRange(needRange.TextRange));
            }

            return null;
        }

        private TreeTextRange GetTreeTextRange(TextRange textRange)
        {
            return new TreeTextRange(new TreeOffset(textRange.StartOffset), new TreeOffset(textRange.EndOffset));
        }

        private string GetLanguageFromNode(ITreeNode node)
        {
            return node == null ? null : node.UserData.GetData(Constants.YcLanguage);
        }
    }
}
