using JetBrains.ReSharper.Intentions.CSharp.Test;
using NUnit.Framework;

namespace ApproximatorTester.Tests
{
    [TestFixture]
    public class ExecuteCSharpRecursiveFsaTests : CSharpContextActionExecuteTestBase<BuildFsaForCSharp>
    {
        protected override string ExtraPath
        {
            get { return "CSharpRecursiveFsa"; }
        }

        protected override string RelativeTestDataPath
        {
            get { return "CSharpRecursiveFsa"; }
        }

        [Test]
        public void TestSimpleUserDefMethod()
        {
            DoTestFiles("SimpleUserDefMethod.cs");
        }
    }
}