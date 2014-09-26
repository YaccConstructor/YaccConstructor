using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RShT1
{
    class Example3
    {
        private void Go(int cond)
        {
            var x = "234";
            /*while(cond > 1)
            {
                 x = x + "+2";
            }*/
            if (cond > 1)
            {
                if (cond > 2)
                {
                    x = "44 / 3 + ";
                }
                else
                {
                    x = "55 ** 4 + ";
                }
                Program.Eval(x + "123 + " + " 23 ** 3 + 14");
            }
            Program.Eval(x + "+3");
        }
    }
}
