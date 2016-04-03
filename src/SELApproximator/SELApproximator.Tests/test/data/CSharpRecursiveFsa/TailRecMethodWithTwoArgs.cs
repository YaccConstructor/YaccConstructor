{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string query1 = Program.CreateStr("x", "y");
            Program.ExecuteImmediate(query1);
        }

        static string CreateStr(string x, string y)
        {
            if (x.IsNormalized())
            {
                return x + y;
            }
            if (y.Length > 2)
            {
                var upd1 = x + "b";
                return Program.CreateStr(upd1, y);
            }
            var upd2 = y + "c";
            return Program.CreateStr(x, upd2);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}