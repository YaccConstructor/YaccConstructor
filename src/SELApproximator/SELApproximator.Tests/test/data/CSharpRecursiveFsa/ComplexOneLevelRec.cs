{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            var query0 = (1 > 0) ? "true" : "false";
            var query1 = "a";
            string query2 = Program.genString();
            string query3 = Program.genStringWithArg("d");
            query1 += query2;
            query1 = query1 + query3;
            SomeClass someClass = new SomeClass();
            for (int i = 0; i != 6; i++)
            {
                query1 += someClass.GenStrMethod();
            }
            string query5 = someClass.GenStrMethodWithArg("g" + query1);
            string query6 = someClass.GenStrMethodWithArgs(query5, query1);
            query6 += Program.withMultiReturn();
            Program.ExecuteImmediate(query6);
        }

        static void ExecuteImmediate(string query)
        {
            
        }

        static string genString()
        {
            var res = "b";
            return res;
        }

        static string genStringWithArg(string s)
        {
            var res = "c" + s;
            return res;
        }

        static string withMultiReturn()
        {
            string s = "y";
            var res = "";
            if (s.IsNormalized())
            {
                res = "x";
                return res;
            }
            res = s + "z";
            return res;
        }
    }

    public class SomeClass
    {
        public string GenStrMethod()
        {
            var res = "e";
            return res;
        }
        
        public string GenStrMethodWithArg(string s)
        {
            var res = "f" + s;
            return res;
        }

        public string GenStrMethodWithArgs(string s1, string s2)
        {
            var res = "h" + s1 + "j" + s2;
            return res;
        }
    }
}