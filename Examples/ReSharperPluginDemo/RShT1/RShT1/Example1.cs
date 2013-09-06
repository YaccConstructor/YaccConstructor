using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace RShT1
{
    class TSQLDEmo
    {
        private void Go(int cond)
        {
            var stmt = "drop procedure";
            if (1 == cond)
            {
                Program.ExecuteImmediate(stmt + "prc2");
            }
            else
            {
                Program.ExecuteImmediate(stmt + "prc1");
            }
            Program.ExecuteImmediate("select x from y where" + "v > b" );
            Program.Eval("123 +" + " 23 ** 3 +  14");
            Program.Eval("123 +" + " 14");
            Program.Eval("123 +" + " -14");
            var add = "+ 2 ";
            if (cond < 1)
            {
                add = "+";
            }
            Program.Eval("123 +(" + " 14)" + add);
        }
    }
}
