{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string query = "a";
            for (int i = 0; i < 10; i++)
            {
                string fieldName = "b";
                query += fieldName;
            }
            Program.ExecuteImmediate(query);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}