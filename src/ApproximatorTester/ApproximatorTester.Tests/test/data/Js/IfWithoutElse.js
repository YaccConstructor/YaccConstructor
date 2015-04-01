{caret}function main(cond)
{
    var query1 = "Select field From ";
    if (cond)
    {
        query1 += "Table1";
    }
    var filter = "where field is numeric";
    query1 += filter;
    execScript(query1);
}
