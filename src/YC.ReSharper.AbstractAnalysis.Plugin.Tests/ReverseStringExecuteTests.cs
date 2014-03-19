using JetBrains.ProjectModel;
using JetBrains.ReSharper.Feature.Services.CSharp.Bulbs;
using JetBrains.ReSharper.Intentions.CSharp.Test;
using JetBrains.ReSharper.Intentions.Extensibility;
using JetBrains.TextControl;
using NUnit.Framework;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Core.Tests
{
  [TestFixture]
    public class ReverseStringExecuteTests<T> : CSharpContextActionExecuteTestBase<T> where T : class, IContextAction
  {
    protected override string ExtraPath
    {
      get { return "ReverseStringAction"; }
    }

    protected override string RelativeTestDataPath
    {
      get { return "ReverseStringAction"; }
    }

    //protected override T CreateContextAction(ISolution solution, ITextControl textControl)
    //{
    //    return new ReverseStringAction(solution.);
    //}

    [Test]
    public void ExecuteTest()
    {
      DoTestFiles("execute01.cs");
    }
  }
}
