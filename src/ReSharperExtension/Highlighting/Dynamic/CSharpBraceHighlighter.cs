using System;

using JetBrains.Annotations;
using JetBrains.DataFlow;
using JetBrains.ReSharper.Daemon.CaretDependentFeatures;
using JetBrains.ReSharper.Feature.Services.ContextActions;
using JetBrains.ReSharper.Feature.Services.Contexts;
using JetBrains.ReSharper.Feature.Services.CSharp.Bulbs;
using JetBrains.ReSharper.Psi.CSharp.Parsing;

namespace ReSharperExtension.Highlighting.Dynamic
{
    [ContainsContextConsumer]
    public class CSharpBraceHighlighter : BaseBraceHighlighter
    {
        protected CSharpBraceHighlighter(IContextActionDataProvider provider)
            : base(provider)
        {
            stringLiteral = CSharpTokenType.STRING_LITERAL_REGULAR;
        }

        [AsyncContextConsumer]
        public static Action ProcessDataContext(
            Lifetime lifetime,
            [NotNull, ContextKey(typeof(CSharpContextActionDataProvider.ContextKey))] IContextActionDataProvider dataProvider,
            InvisibleBraceHintManager invisibleBraceHintManager,
            MatchingBraceSuggester matchingBraceSuggester,
            HighlightingProlongedLifetime prolongedLifetime)
        {
            return new CSharpBraceHighlighter(dataProvider).ProcessDataContextImpl(lifetime, prolongedLifetime, dataProvider, invisibleBraceHintManager, matchingBraceSuggester);
        }
    }
}
