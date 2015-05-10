{caret}function main()
{
    var query = "a";
    for (var i = 0; i < 10; i++)
    {
        var iStr = "b";
        for (var j = 0; j < 10; j++)
        {
            query += iStr;   
        }
    }
    execScript(query);
}