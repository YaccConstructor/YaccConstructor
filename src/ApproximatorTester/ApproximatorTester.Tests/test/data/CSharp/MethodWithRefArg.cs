{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string query = "ab";
            string tableName = "b";
            query = query.Replace(tableName, "c");
            Program.ExecuteImmediate(query);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}
