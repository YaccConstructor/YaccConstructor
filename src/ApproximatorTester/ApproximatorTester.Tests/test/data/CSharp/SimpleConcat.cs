{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string query1 = "Select field From ";
            string query2 = query1 + "Table";
            Program.ExecuteImmediate(query2);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}
