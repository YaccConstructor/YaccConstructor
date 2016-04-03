{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string query1 = Program.CreateStr();
            Program.ExecuteImmediate(query1);
        }

        static string CreateStr()
        {
            string s = "a";
            return s;
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}