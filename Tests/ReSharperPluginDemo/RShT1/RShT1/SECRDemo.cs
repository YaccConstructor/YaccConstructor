using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RShT1
{
    class SECRDemo
    {

    public static void Execute()
    {
        Program.Eval("(1 + 2) * 3");
        Program.ExecuteImmediate("insert into y (x,v) values (1,2)");
        string query = "select x from y where";
        Program.ExecuteImmediate(query + "v > 1");
    }

    public static void Go(bool cond)
    {
        string query = "xxx = 1;";
        if (cond)
        {
            query += "yyy = 2;";
        }
        query += "z = xxx + yyy;";
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
    }
}
