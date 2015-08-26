using System;

using Microsoft.FSharp.Collections;

using JetBrains.Application.Progress;
using JetBrains.ProjectModel;
using JetBrains.ReSharper.Feature.Services.ContextActions;
using JetBrains.ReSharper.Feature.Services.CSharp.Analyses.Bulbs;
using JetBrains.TextControl;
using JetBrains.Util;

using YC.ReSharper.AbstractAnalysis.LanguageApproximation;

namespace SELApproximator
{
    [ContextAction(Name = "BuildFSAForCSharp", Description = "BuildFSAForCSharp", Group = "C#")]
    public class BuildFsaForCSharp : ContextActionBase
    {
        private readonly ICSharpContextActionDataProvider _provider;
        private readonly Hotspot.Hotspot _hotspot = new Hotspot.Hotspot("calc", "Program", "Eval", 0, "void");

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
            const int recursionMaxLevel = 3; // 0 for top and 3 level down
            
            var tuple = new Tuple<string, Hotspot.Hotspot>(_hotspot.Language, _hotspot);
            var temp = new FSharpList<Tuple<string, Hotspot.Hotspot>>(tuple, null);

            var fsa = ApproximateCsharp.ApproximateFileWithLogging(inputFile, recursionMaxLevel, temp)[0].Item2;
            Utils.OutputCSharpResult(Utils.FsaToTestDot(fsa), _provider);
            return null;
        }

        public override string Text
        {
            get { return "BuildFSAForCSharp"; }
        }
    }
}