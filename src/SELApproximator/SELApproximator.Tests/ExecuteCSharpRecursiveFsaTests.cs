using JetBrains.ReSharper.FeaturesTestFramework.Intentions;
using NUnit.Framework;

namespace SELApproximator.Tests
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
        public void TestTailRecMethod()
        {
            DoTestFiles("TailRecMethod.cs");
        }

        [Test]
        public void TestTailRecMethodTwoExits()
        {
            DoTestFiles("TailRecMethodTwoExits.cs");
        }

        [Test]
        public void TestTailRecMethodWithTwoArgs()
        {
            DoTestFiles("TailRecMethodWithTwoArgs.cs");
        }
        
    }
}