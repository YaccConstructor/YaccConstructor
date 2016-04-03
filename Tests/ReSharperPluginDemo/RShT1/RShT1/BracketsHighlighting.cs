namespace RShT1
{
    class BracketsHighlighting
    {
        public void Do(bool cond)
        {
            //brackets are tokens
            Program.Eval("(1 + 2) * 3");

            //brackets are literals
            Program.ExecuteImmediate("insert into y (x,v) values (1,2)");

            //embedded brackets 
            Program.Eval("( 2 * ( 4 + 5 ) / 9)");

            //finding need range and bracket
            string fourteen = "14)";
            Program.Eval("123 *( 1 - " + fourteen);

            //one-to-many (more than one) matching
            string insertQuery = "insert  into y(x";

            if (cond)
                insertQuery += ", v)";
            else
                insertQuery = insertQuery + ", u)";
            insertQuery += " values (1, 2)";
            Program.ExecuteImmediate(insertQuery);
        }
    }
}
