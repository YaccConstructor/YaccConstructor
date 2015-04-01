{caret}function main(cond)
{
    var query1 = "Select";
    var field = " fieldName ";
    query1 += "from";
    var tableName = "Table1";
    if (cond)
    {
        query1 += tableName;
    }
    else
    {
        tableName = "Table2";
        query1 += tableName;
    }
    var filter = "where field is numeric";
    query1 += filter;
    execScript(query1);
}
