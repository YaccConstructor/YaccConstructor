namespace RShT1
{
    class ExtCalc
    {
        private void Go(bool cond)
        {
            Program.ExtEval("x = x;");

            Program.ExtEval("x = y; y = x;");

            string query0 = "x = 1;";
            if (cond)
            {
                query0 += "y = 2;";
            }
            query0 += "z = x + y;";
            Program.ExtEval(query0);


            string query = "x = 1;";
            if (cond)
            {
                query += "y = 2;";
            }
            else
            {
                query += "z = 3;";
            }

            query += "w = y + z;";
            Program.ExtEval(query);
        }
    }
}
