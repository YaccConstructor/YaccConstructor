using System;
using JetBrains.Application.Progress;
using JetBrains.ProjectModel;
using JetBrains.ReSharper.Feature.Services.Bulbs;
using JetBrains.ReSharper.Feature.Services.CSharp.Bulbs;
using JetBrains.ReSharper.Intentions.Extensibility;
using JetBrains.TextControl;
using JetBrains.Util;
using YC.ReSharper.AbstractAnalysis.LanguageApproximation;

namespace ApproximatorTester
{
    [ContextAction(Name = "BuildFSAForCSharp", Description = "BuildFSAForCSharp", Group = "C#")]
    public class BuildFsaForCSharp : ContextActionBase
    {
        private readonly ICSharpContextActionDataProvider _provider;

        public BuildFsaForCSharp(ICSharpContextActionDataProvider provider)
        {
            _provider = provider;
        }

        public override bool IsAvailable(IUserDataHolder cache)
        {
            return true;
        }

        protected override Action<ITextControl> ExecutePsiTransaction(ISolution solution, IProgressIndicator progress)
        {
            var inputFile = _provider.PsiFile;
            var fsa = ApproximateCsharp.BuildApproximation(inputFile);
            Utils.OutputCSharpResult(Utils.FsaToTestDot(fsa), _provider);
            return null;
        }

        public override string Text
        {
            get { return "BuildFSAForCSharp"; }
        }
    }
}