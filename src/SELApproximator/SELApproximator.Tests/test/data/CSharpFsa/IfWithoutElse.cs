{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string query1 = "a";
            if (args.Length != 0)
            {
                query1 += "b";
            }
            string filter = "c";
            query1 += filter;
            Program.ExecuteImmediate(query1);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}
