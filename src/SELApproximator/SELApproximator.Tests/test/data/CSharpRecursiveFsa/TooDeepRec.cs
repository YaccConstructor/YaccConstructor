{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            string query1 = Program.GetStr1();
            Program.ExecuteImmediate(query1);
        }

        static string GetStr1()
        {
            string s = "a" + Program.GetStr2();
            return s;
        }

        static string GetStr2()
        {
            string s = "b" + Program.GetStr3();
            return s;
        }

        static string GetStr3()
        {
            string s = "c" + Program.GetStr4();
            return s;
        }

        static string GetStr4()
        {
            string s = "will not be approximated";
            return s;
        }

        static void ExecuteImmediate(string query)
        {
            
        }
    }
}