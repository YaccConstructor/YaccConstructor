{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string query1 = "Select field From ";
            if (args.Length != 0)
            {
                query1 += "Table1";
            }
            string filter = "where field is numeric";
            query1 += filter;
            Program.ExecuteImmediate(query1);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}
