{caret}function main(cond)
{
    var query = "select";
    for (var i = 0; i < 10 && i > 0 || query.IsNormalized(); i++)
    {
        var localQ = "from ";
        if (cond)
        {
            localQ += "Table1";
        }
        else
        {
            localQ += "Table2";
        }
        query += localQ;
    }
    execScript(query);
}
