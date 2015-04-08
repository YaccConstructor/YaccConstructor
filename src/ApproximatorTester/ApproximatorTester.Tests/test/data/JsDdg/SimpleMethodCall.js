{caret}function main(cond)
{
    var query = "Select field From Table1";
    query = query.replace("Table1", "Table2");
    execScript(query);
}