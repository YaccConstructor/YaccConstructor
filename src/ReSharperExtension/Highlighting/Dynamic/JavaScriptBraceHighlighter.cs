using System;

using JetBrains.Annotations;
using JetBrains.DataFlow;
using JetBrains.ReSharper.Daemon.CaretDependentFeatures;
using JetBrains.ReSharper.Feature.Services.ContextActions;
using JetBrains.ReSharper.Feature.Services.Contexts;
using JetBrains.ReSharper.Feature.Services.JavaScript.Bulbs;
using JetBrains.ReSharper.Psi.JavaScript.Parsing;

namespace ReSharperExtension.Highlighting.Dynamic
{
    [ContainsContextConsumer]
    public class JavaScriptBraceHighlighter : BaseBraceHighlighter
    {
        protected JavaScriptBraceHighlighter(IContextActionDataProvider provider)
            : base(provider)
        {
            stringLiteral = JavaScriptTokenType.STRING_LITERAL;
        }

        [AsyncContextConsumer]
        public static Action ProcessDataContext(
            Lifetime lifetime,
            [NotNull, ContextKey(typeof(JavaScriptContextActionDataProvider.ContextKey))] IContextActionDataProvider dataProvider,
            InvisibleBraceHintManager invisibleBraceHintManager,
            MatchingBraceSuggester matchingBraceSuggester,
            HighlightingProlongedLifetime prolongedLifetime)
        {
            return new JavaScriptBraceHighlighter(dataProvider).ProcessDataContextImpl(lifetime, prolongedLifetime, dataProvider, invisibleBraceHintManager, matchingBraceSuggester);
        }
    }
}
