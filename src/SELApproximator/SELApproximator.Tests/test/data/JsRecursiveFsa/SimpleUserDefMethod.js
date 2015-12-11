{caret}function namespace()
{
    function createStr()
    {
        var s = "a";
        return s;
    }

    function main(args)
    {
        var query1 = createStr();
        execScript(query1);
    }
}