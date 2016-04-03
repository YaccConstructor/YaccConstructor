namespace RShT1
{
    class EmbeddedCalc
    {
        static int Calculate(bool cond)
        {
            var expr = "1+(2";
            var baseMult1 = "4";
            var baseMult2 = "3";
            if (cond)
            {
                expr = expr + "*" + baseMult1;
            }
            else
            {
                expr = expr + "/" + baseMult2;
            }
            return Program.Eval(expr + ")");
        }
    }
}
