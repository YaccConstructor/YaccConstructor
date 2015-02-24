{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string query1 = "Select * From ";
            if (args.Length != 0)
            {
                query1 += "Table1";
            }
            else
            {
                query1 += "Table2";
            }
            Program.ExecuteImmediate(query1);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}
