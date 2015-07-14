using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RShT1
{
    class CalcWithCyclesAndCalls
    {
        static int Calculate(bool cond)
        {
            var expr = "(10";
            for (int i = 0; i < 10; ++i)
            {
                expr += "+ 1";
                if (cond)
                {
                    expr += CalcWithCyclesAndCalls.TrueCaseStr();
                }
                else
                {
                    expr += CalcWithCyclesAndCalls.FalseCaseStr();
                }
            }
            var logMsg = "Calculaton starts";
            Console.Out.WriteLine(logMsg);
            return Program.Eval(expr + ") / 2");
        }

        static string TrueCaseStr()
        {
            return "* 3";
        }

        static string FalseCaseStr()
        {
            return "* 2";
        }
    }
}
