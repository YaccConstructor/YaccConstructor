{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            var query0 = (1 > 0) ? "true" : "false";
            var query1 = "Select * From TableName";
            string query2 = genString();
            string query3 = genStringWithArg("hello");
            SomeClass someClass = new SomeClass();
            string query4 = someClass.GenStrMethod();
            string query5 = someClass.GenStrMethodWithArg("arg" + query1);
            string query6 = someClass.GenStrMethodWithArgs("arg", query1);
            Program.ExecuteImmediate(query1);
        }

        static void ExecuteImmediate(string query)
        {
            
        }

        static string genString()
        {
            return "some str";
        }

        static string genStringWithArg(string s)
        {
            return "arg: " + s;
        }

        static string withMultiReturn()
        {
            string s = "";
            if (s.IsNormalized())
            {
                return "norm";
            }
            string res = s + "some";
            return res;
        }
    }

    public class SomeClass
    {
        public string GenStrMethod()
        {
            return "some str from method";
        }
        
        public string GenStrMethodWithArg(string s)
        {
            return "arg: " + s;
        }

        public string GenStrMethodWithArgs(string s1, string s2)
        {
            return "args: " + s1 + " " + s2;
        }
    }
}
