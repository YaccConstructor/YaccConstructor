using JetBrains.ReSharper.Intentions.CSharp.Test;
using NUnit.Framework;

namespace ApproximatorTester.Tests
{
    [TestFixture]
    public class ExecuteCSharpTests : CSharpContextActionExecuteTestBase<RunCSharpApproximator>
    {
        protected override string ExtraPath
        {
            get { return "CSharp"; }
        }

        protected override string RelativeTestDataPath
        {
            get { return "CSharp"; }
        }

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

        //[Test]
        //public void TestSimpleFor()
        //{
        //    DoTestFiles("SimpleFor.cs");
        //}

        //[Test]
        //public void TestForWithLocalVar()
        //{
        //    DoTestFiles("ForWithLocalVar.cs");
        //}

        //[Test]
        //public void TestComplexConditionFor()
        //{
        //    DoTestFiles("ComplexConditionFor.cs");
        //}

        //[Test]
        //public void TestNestedFor()
        //{
        //    DoTestFiles("NestedFor.cs");
        //}

        //[Test]
        //public void TestForWithIf()
        //{
        //    DoTestFiles("ForWithIf.cs");
        //}

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
    }
}