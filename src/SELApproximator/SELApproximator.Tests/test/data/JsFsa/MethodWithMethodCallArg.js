{caret}function main(cond)
{
    var innerQuery = "ab";
    var tableName = "b";
    var query = "cd";
    query = query.replace("d", innerQuery.replace(tableName, "e"));
    execScript(query);
}