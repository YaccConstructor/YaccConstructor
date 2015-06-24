using JetBrains.ReSharper.FeaturesTestFramework.Intentions;
using NUnit.Framework;

namespace SELApproximator.Tests
{
    [TestFixture]
    public class ExecuteJsFsaTests : CSharpContextActionExecuteTestBase<BuildFsaForJs>
    {
        protected override string ExtraPath
        {
            get { return "JsFsa"; }
        }

        protected override string RelativeTestDataPath
        {
            get { return "JsFsa"; }
        }

        [Test]
        public void TestSimpleQuery()
        {
            DoTestFiles("SimpleQuery.js");
        }

        [Test]
        public void TestSimpleFor()
        {
            DoTestFiles("SimpleFor.js");
        }

        [Test]
        public void TestNestedFor()
        {
            DoTestFiles("NestedFor.js");
        }

        [Test]
        public void TestForWithLocalVar()
        {
            DoTestFiles("ForWithLocalVar.js");
        }

        [Test]
        public void TestForWithIf()
        {
            DoTestFiles("ForWithIf.js");
        }

        [Test]
        public void TestFourVars()
        {
            DoTestFiles("FourVars.js");
        }

        [Test]
        public void TestIfWithoutElse()
        {
            DoTestFiles("IfWithoutElse.js");
        }

        [Test]
        public void TestMethodWithMethodCallArg()
        {
            DoTestFiles("MethodWithMethodCallArg.js");
        }

        [Test]
        public void TestMethodWithRefArg()
        {
            DoTestFiles("MethodWithRefArg.js");
        }

        [Test]
        public void TestSimpleMethodCall()
        {
            DoTestFiles("SimpleMethodCall.js");
        }

        [Test]
        public void TestSimpleConcat()
        {
            DoTestFiles("SimpleConcat.js");
        }
    }
}