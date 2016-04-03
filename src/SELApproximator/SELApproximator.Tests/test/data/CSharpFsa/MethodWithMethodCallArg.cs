{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string innerQuery = "ab";
            string tableName = "b";
            string query = "cd";
            query = query.Replace("d", innerQuery.Replace(tableName, "e"));
            Program.ExecuteImmediate(query);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}
