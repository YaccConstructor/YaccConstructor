namespace RShT1
{
    class Example5
    {
        private void Go(int cond)
        {
            string whereCond;
            if (1 == cond)
            {
                whereCond = "a < b";
            }
            else
            {
                whereCond = "v > 5";
            }

            //Program.ExecuteImmediate("select x from y where " + whereCond);
            //Program.ExecuteImmediate("select x from y where (v > 4)  and b < 2");

            string insStmt = "insert into x(a, b, c, d)";
            Program.ExecuteImmediate(insStmt + "values(@a, @b, @c, @d)");

            string stmt;
            if (cond > 0)
            {
                stmt = "select * from y";
            }
            else
            {
                stmt = "select x from y";
            }
            Program.ExecuteImmediate(stmt + "where" + whereCond);
            Program.ExecuteImmediate(stmt + " whe"+"re a > 3");
        }
    }
}
