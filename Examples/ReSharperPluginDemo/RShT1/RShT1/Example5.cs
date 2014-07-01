using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RShT1
{
    class Example5
    {
        private void Go(int cond)
        {
//            Program.ExecuteImmediate("select x from y where v > b");
            Program.ExecuteImmediate("select x from from y");
//            Program.ExecuteImmediate("select x from y where v > b and 1 == 2");
            Program.ExecuteImmediate("select x from y where v > b where v > b");
        }
    }
}
