{caret}function main(cond) {
    var query1 = "a";
    if (cond)
    {
        query1 += "b";
    }
    else
    {
        query1 += "c";
    }
    var filter = "d";
    query1 += filter;
    execScript(query1);
}