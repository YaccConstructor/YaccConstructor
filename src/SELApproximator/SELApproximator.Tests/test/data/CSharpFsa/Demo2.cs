{caret}namespace N
{
    class Program
    {
        public void Do(bool cond)
        {
            string insertQuery = "insert  into y(x";

            if (cond)
                insertQuery += ", v) ";
            else
                insertQuery = insertQuery + ", u)";
            insertQuery += " values (1, 2)";
            Program.ExecuteImmediate(insertQuery);
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}