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
            var updated = s + "b";
            return Program.CreateStr(updated);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}