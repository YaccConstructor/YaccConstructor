using System;
using JetBrains.Application.Progress;
using JetBrains.ProjectModel;
using JetBrains.ReSharper.Feature.Services.Bulbs;
using JetBrains.ReSharper.Feature.Services.JavaScript.Bulbs;
using JetBrains.ReSharper.Intentions.Extensibility;
using JetBrains.TextControl;
using JetBrains.Util;
using YC.ReSharper.AbstractAnalysis.LanguageApproximation;

namespace ApproximatorTester
{
    [ContextAction(Name = "RunApproximatorJs", Description = "Run Approximator for Js", Group = "JS")]
    public class RunJsApproximator : ContextActionBase
    {
        private readonly IJavaScriptContextActionDataProvider _provider;

        public RunJsApproximator(IJavaScriptContextActionDataProvider provider)
        {
            _provider = provider;
        }

        public override bool IsAvailable(IUserDataHolder cache)
        {
            return true;
        }

        protected override Action<ITextControl> ExecutePsiTransaction(ISolution solution, IProgressIndicator progress)
        {
            var jsCfg = _provider.GetControlFlowGraf();
            ApproximationBuilderJs.BuildApproximation(jsCfg);
            return null;
        }

        public override string Text
        {
            get { return "Run Approximator for Js"; }
        }
    }
}