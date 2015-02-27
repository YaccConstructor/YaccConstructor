using JetBrains.ReSharper.Intentions.CSharp.Test;
using NUnit.Framework;

namespace ApproximatorTester.Tests
{
    [TestFixture]
    public class ExecuteTests : CSharpContextActionExecuteTestBase<RunApproximatorAction>
    {
        protected override string ExtraPath
        {
            get { return "AllTests"; }
        }

        protected override string RelativeTestDataPath
        {
            get { return "AllTests"; }
        }

        [Test]
        public void Test0()
        {
            DoTestFiles("test0.cs");
        }

        [Test]
        public void Test1()
        {
            DoTestFiles("test1.cs");
        }
    }
}