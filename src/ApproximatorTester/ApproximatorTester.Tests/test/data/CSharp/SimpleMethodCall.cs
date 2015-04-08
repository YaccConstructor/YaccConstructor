{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string query = "aba";
            query = query.Replace("a", "c");
            Program.ExecuteImmediate(query);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}
