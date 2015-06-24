using JetBrains.ReSharper.FeaturesTestFramework.Intentions;
using NUnit.Framework;

namespace SELApproximator.Tests
{
    [TestFixture]
    public class ExecuteJsRecursiveFsaTests : CSharpContextActionExecuteTestBase<BuildFsaForJs>
    {
        protected override string ExtraPath
        {
            get { return "JsRecursiveFsa"; }
        }

        protected override string RelativeTestDataPath
        {
            get { return "JsRecursiveFsa"; }
        }

        //[Test]
        //public void TestSimpleUserDefMethod()
        //{
        //    DoTestFiles("SimpleUserDefMethod.js");
        //}

        //[Test]
        //public void TestComplexOneLevelRec()
        //{
        //    DoTestFiles("ComplexOneLevelRec.js");
        //}
    }
}