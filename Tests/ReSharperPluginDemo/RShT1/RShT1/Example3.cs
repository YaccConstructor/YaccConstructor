namespace RShT1
{
    class Example3
    {
        private void Go(int cond)
        {
            var x = "234";
            var y = "534";
            if (cond > 1)
            {
                if (cond > 2)
                {
                    x = "44 + 3 + ";
                }
                else
                {
                    x = "55 ** 4 + ";
                }
                if (cond > 4)
                {
                    y = " +  44 + 3";
                }
                else
                {
                    y = " * 55 ** 4 + 2";
                }
                Program.Eval(x + "123 + " + " 23 ** 3 + 14" + y);
            }
            Program.Eval(x + "123 + " + " 23 ** 3 + 14" + y);
        }
    }
}
