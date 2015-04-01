{caret}function main()
{
    var query = "select";
    for (var i = 0; i < 10; i++)
    {
        var iStr = "i";
        for (var j = 0; j < 10; j++)
        {
            query += iStr;   
        }
    }
    execScript(query);
}
