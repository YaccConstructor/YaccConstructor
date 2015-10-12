using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RShT1
{
    class SimpleConcat
    {
        private void Go(int cond)
        {
            string query1 = "a";
            Program.ExecuteImmediate(query1 + "b");
        }
    }
}
