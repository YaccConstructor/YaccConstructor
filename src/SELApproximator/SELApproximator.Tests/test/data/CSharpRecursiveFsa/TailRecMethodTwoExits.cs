{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string query1 = Program.CreateStr("a");
            Program.ExecuteImmediate(query1);
        }

        static string CreateStr(string s)
        {
            if (s.IsNormalized())
            {
                return s;
            }
            if (s.Length > 2)
            {
                var upd1 = s + "a";
                return Program.CreateStr(upd1);
            }
            var upd2 = s + "b";
            return Program.CreateStr(upd2);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}