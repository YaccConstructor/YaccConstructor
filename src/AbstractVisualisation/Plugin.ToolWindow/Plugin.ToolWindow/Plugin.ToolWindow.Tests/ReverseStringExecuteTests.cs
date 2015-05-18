using JetBrains.ReSharper.Intentions.CSharp.Test;
using NUnit.Framework;

namespace Plugin.ToolWindow.Tests
{
    [TestFixture]
    public class ReverseStringExecuteTests : CSharpContextActionExecuteTestBase<ReverseStringAction>
    {
        protected override string ExtraPath
        {
            get { return "ReverseStringAction"; }
        }

        protected override string RelativeTestDataPath
        {
            get { return "ReverseStringAction"; }
        }

        [Test]
        public void ExecuteTest()
        {
            DoTestFiles("execute01.cs");
        }
    }
}