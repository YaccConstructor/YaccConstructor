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

        [Test]
        public void TestComplexOneLevelRec()
        {
            DoTestFiles("ComplexOneLevelRec.cs");
        }

        [Test]
        public void TestThreeLevelRec()
        {
            DoTestFiles("ThreeLevelRec.cs");
        }

        [Test]
        public void TestTooDeepRec()
        {
            DoTestFiles("TooDeepRec.cs");
        }

        [Test]
        public void TestSimpleUserDefRecMethod()
        {
            DoTestFiles("SimpleUserDefRecMethod.cs");
        }
    }
}