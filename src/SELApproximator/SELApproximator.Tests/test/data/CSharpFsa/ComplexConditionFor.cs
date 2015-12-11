{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string query = "a";
            for (int i = 0; i < 10 && i > 0 || query.IsNormalized(); i++)
            {
                query += "b";
            }
            Program.ExecuteImmediate(query);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}