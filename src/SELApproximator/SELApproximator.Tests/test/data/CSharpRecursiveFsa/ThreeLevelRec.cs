{caret}namespace N
{
    class Program
    {
        static void Main(string[] args)
        {
            var q = "a";
            q += Program.genStringWithArg(q);
            SomeClass sc = new SomeClass();
            q += sc.GenStrMethodWithArgs(q, q);
            Program.ExecuteImmediate(q);
        }

        static void ExecuteImmediate(string query)
        {
            
        }

        static public string genStringWithArg(string s)
        {
            return s + Program.genString();
        }

        static public string genString()
        {
            return "b";
        }
    }

    public class SomeClass
    {
        public string GenStrMethodWithArgs(string s1, string s2)
        {
            if (s1.IsNormalized())
            {
                var res = s1 + Program.genString();
                return res;
            }
            else
            {
                var res = s1 + Program.genStringWithArg(s2);
                return res;
            }
        }
    }
}