{caret}function main()
{
    var query = "a";
    for (var i = 0; i < 10; i++)
    {
        var fieldName = "b";
        query += fieldName;
    }
    execScript(query);
}