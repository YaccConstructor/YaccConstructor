{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string innerQuery = "Select field From Table1";
            string tableName = "Table1";
            string query = "Do smth";
            query = query.Replace("smth", innerQuery.Replace(tableName, "Table2"));
            Program.ExecuteImmediate(query);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}
