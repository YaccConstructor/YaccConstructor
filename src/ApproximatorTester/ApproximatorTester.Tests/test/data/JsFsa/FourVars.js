{caret}function main(cond)
{
    var query1 = "a";
    var field = " not used ";
    query1 += "b";
    var tableName = "c";
    if (cond)
    {
        query1 += tableName;
    }
    else
    {
        tableName = "d";
        query1 += tableName;
    }
    var filter = "e";
    query1 += filter;
    execScript(query1);
}