using System;

namespace RShT1
{
    class SECRDemo
    {
        public static void Execute()
        {
            Program.Eval("(1 + 2) * 3");
            Program.ExecuteImmediate("insert into y (x,v) values (1,2)");
            string query = "select x from y where";
            Program.ExecuteImmediate(query + " v > 1");
        }

        public static void Go(bool cond)
        {
            var query = "varX = 1;";
            if (cond)
                query += "varY = 2;";
            query += "varZ = varX + varY;";
            Program.ExtEval(query);
        }

        public static void Insert(bool cond)
        {
            string insertQuery = "insert  into y(x";

            if (cond)
                insertQuery += ", v)";
            else
                insertQuery = insertQuery + ", u)";
            insertQuery += " values (1, 2)";
            Program.ExecuteImmediate(insertQuery);
        }

        public static int Calculate(bool cond)
        {
            var expr = "(10";
            for (int i = 0; i < 10; ++i)
            {
                expr += " + 1";
                if (cond)
                    expr += SECRDemo.TrueCaseStr();
                else
                    expr += SECRDemo.FalseCaseStr();
            }
            var logMsg = "Calculation starts";
            Console.Out.WriteLine(logMsg);
            return Program.Eval(expr + ") /2");
        }

        static string TrueCaseStr()
        {
            return "*3";
        }

        static string FalseCaseStr()
        {
            return "*2";
        }

    }
}
