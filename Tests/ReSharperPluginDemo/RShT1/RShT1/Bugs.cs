using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RShT1
{
    public class Bugs
    {
        public void Do(bool cond)
        {
            string fourteen = "14)";
            Program.Eval("123 *( 1 - " + fourteen);

            string insertQuery = "insert  into y(x";

            if (cond)
                insertQuery += ", v) ";
            else
                insertQuery = insertQuery + ", u)";
            insertQuery += " values (1, 2)";
            Program.ExecuteImmediate(insertQuery);
        }
    }
}
