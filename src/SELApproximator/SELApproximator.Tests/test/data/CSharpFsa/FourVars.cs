{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string query1 = "a";
            const string field = " not used ";
            query1 += "b";
            var tableName = "c";
            if (args.Length != 0)
            {
                query1 += tableName;
            }
            else
            {
                tableName = "d";
                query1 += tableName;
            }
            string filter = "e";
            query1 += filter;
            Program.ExecuteImmediate(query1);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}
