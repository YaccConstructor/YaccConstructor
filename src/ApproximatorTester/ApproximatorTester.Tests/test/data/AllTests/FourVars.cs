{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string query1 = "Select";
            const string field = " fieldName ";
            query1 += "from";
            var tableName = "Table1";
            if (args.Length != 0)
            {
                query1 += tableName;
            }
            else
            {
                tableName = "Table2";
                query1 += tableName;
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
