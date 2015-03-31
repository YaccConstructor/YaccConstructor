using JetBrains.ReSharper.Intentions.CSharp.Test;
using NUnit.Framework;

namespace ApproximatorTester.Tests
{
    [TestFixture]
    public class ExecuteJsTests : CSharpContextActionExecuteTestBase<RunJsApproximator>
    {
        protected override string ExtraPath
        {
            get { return "Js"; }
        }

        protected override string RelativeTestDataPath
        {
            get { return "Js"; }
        }

        [Test]
        public void TestA()
        {
            DoTestFiles("A.js");
        }
    }
}