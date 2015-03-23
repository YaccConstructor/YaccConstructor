{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string query = "select";
            for (int i = 0; i < 10 && i > 0 || query.IsEmpty(); i++)
            {
                string localQ = "from ";
                if (args.Length != 0)
                {
                    localQ += "Table1";
                }
                else
                {
                    localQ += "Table2";
                }
                query += localQ;
            }
            Program.ExecuteImmediate(query);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}
