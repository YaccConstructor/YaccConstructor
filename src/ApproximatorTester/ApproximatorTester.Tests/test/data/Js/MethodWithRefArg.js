{caret}function main(cond)
{
    var query = "Select field From Table1";
    var tableName = "Table1";
    query = query.replace(tableName, "Table2");
    execScript(query);
}