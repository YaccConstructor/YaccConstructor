{caret}function main(cond)
{
    var query = "ab";
    var tableName = "b";
    query = query.replace(tableName, "c");
    execScript(query);
}