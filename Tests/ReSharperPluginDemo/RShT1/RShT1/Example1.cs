namespace RShT1
{
    class TSQLDEmo
    {
        private void Go(int cond)
        {
            var stmt = "drop procedure";
            if (1 == cond)
            {
                Program.ExecuteImmediate(stmt + "prc2");
            }
            else
            {
                Program.ExecuteImmediate(stmt + "prc1");
            }
            var s = "insert into y (x,v) values (1++,2)";
            Program.ExecuteImmediate(s);
            Program.ExecuteImmediate("select x from y where" + "v > b" );
            Program.Eval("123 ++" + " 23 ** 3 +  14");
            Program.Objnotation("{\"Name\" : \"Ivan\"}");
           // Program.Eval("123 +" + " 14");
           // Program.Eval("123 +" + " -14");
            var add = "+ 2 ";
            if (cond < 1)
            {
                add = "+";
            }
           // Program.Eval("123 +(" + " 14)" + add);
        }
    }
}
