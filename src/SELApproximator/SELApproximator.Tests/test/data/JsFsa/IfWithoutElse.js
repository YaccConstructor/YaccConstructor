{caret}function main(cond)
{
    var query1 = "a";
    if (cond)
    {
        query1 += "b";
    }
    var filter = "c";
    query1 += filter;
    execScript(query1);
}