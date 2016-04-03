namespace RShT1
{
    class Calc
    {
        private void Go()
        {
            Program.Eval("123 + 3 - + + " + "14 - 52 * " + " 99*3*4" + "45 + 1");
            Program.Eval("123 +" + " 23 ** 3 + 14");
            Program.Eval("*123 +" + " 14");
            Program.Eval("123 +" + " -14");
            Program.Eval("123 +(" + "  14)");
        }
    }
}
