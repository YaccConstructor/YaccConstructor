{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string query = "Select field From Table1";
            query = query.Replace("Table1", "Table2");
            Program.ExecuteImmediate(query);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}
