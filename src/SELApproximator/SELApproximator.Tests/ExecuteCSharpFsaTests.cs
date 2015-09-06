using JetBrains.ReSharper.FeaturesTestFramework.Intentions;
using NUnit.Framework;

namespace SELApproximator.Tests
{
    [TestFixture]
    public class ExecuteCSharpFsaTests : CSharpContextActionExecuteTestBase<BuildFsaForCSharp>
    {
        protected override string ExtraPath
        {
            get { return "CSharpFsa"; }
        }

        protected override string RelativeTestDataPath
        {
            get { return "CSharpFsa"; }
        }

        // == better way
        //[TestCase("SimpleQuery.cs")]
        //[TestCase("FourVars.cs")]
        //...
        //public void TestCases(string testSrc)
        //{
        //    DoTestFiles(testSrc);
        //}

        [Test]
        public void TestSimpleQuery()
        {
            DoTestFiles("SimpleQuery.cs");
        }

        [Test]
        public void TestFourVars()
        {
            DoTestFiles("FourVars.cs");
        }

        [Test]
        public void TestSimpleMethodCall()
        {
            DoTestFiles("SimpleMethodCall.cs");
        }

        [Test]
        public void TestMethodWithRefArg()
        {
            DoTestFiles("MethodWithRefArg.cs");
        }

        [Test]
        public void TestMethodWithMethodCallArg()
        {
            DoTestFiles("MethodWithMethodCallArg.cs");
        }

        [Test]
        public void TestIfWithoutElse()
        {
            DoTestFiles("IfWithoutElse.cs");
        }

        [Test]
        public void TestSimpleConcat()
        {
            DoTestFiles("SimpleConcat.cs");
        }

        [Test]
        public void TestSimpleFor()
        {
            DoTestFiles("SimpleFor.cs");
        }

        [Test]
        public void TestForWithLocalVar()
        {
            DoTestFiles("ForWithLocalVar.cs");
        }

        [Test]
        public void TestComplexConditionFor()
        {
            DoTestFiles("ComplexConditionFor.cs");
        }

        [Test]
        public void TestNestedFor()
        {
            DoTestFiles("NestedFor.cs");
        }

        [Test]
        public void TestForWithIf()
        {
            DoTestFiles("ForWithIf.cs");
        }

        // Tests for Secr Demo
        [Test]
        public void TestDemo1()
        {
            DoTestFiles("Demo1.cs");
        }

        [Test]
        public void TestDemo2()
        {
            DoTestFiles("Demo2.cs");
        }
    }
}