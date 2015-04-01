{caret}function main(cond)
{
    var innerQuery = "Select field From Table1";
    var tableName = "Table1";
    var query = "Do smth";
    query = query.replace("smth", innerQuery.replace(tableName, "Table2"));
    execScript(query);
}