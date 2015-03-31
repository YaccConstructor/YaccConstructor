{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string query = "select";
            for (int i = 0; i < 10; i++)
            {
                string fieldName = " field";
                query += fieldName;
            }
            Program.ExecuteImmediate(query);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}
