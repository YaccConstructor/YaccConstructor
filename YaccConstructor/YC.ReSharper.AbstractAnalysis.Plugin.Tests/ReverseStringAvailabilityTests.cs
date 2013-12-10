using JetBrains.ReSharper.Feature.Services.CSharp.Bulbs;
using JetBrains.ReSharper.Intentions.CSharp.Test;
using JetBrains.ReSharper.Intentions.Extensibility;
using NUnit.Framework;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Core.Tests
{
  [TestFixture]
  public class ReverseStringAvailabilityTests : CSharpContextActionAvailabilityTestBase
  {
    [Test]
    public void AvailabilityTest()
    {
      DoTestFiles("availability01.cs");
    }

    protected override IContextAction CreateContextAction(ICSharpContextActionDataProvider dataProvider)
    {
      return new ReverseStringAction(dataProvider);
    }

    protected override string ExtraPath
    {
      get { return "ReverseStringAction"; }
    }

    protected override string RelativeTestDataPath
    {
      get { return "ReverseStringAction"; }
    }
  }
}
