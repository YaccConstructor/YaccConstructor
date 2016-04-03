{caret}function namespace()
{
    function genString() 
    {
        var r = "b";
        return r;
    }

    function genStringWithArg(s)
    {
        var r = "c" + s;
        return r;
    }

    function withMultiReturn()
    {
        var s = "y";
        var r = "x";
        if (s.IsNormalized())
        {
            return r;
        }
        r = s + "z";
        return r;
    }

    function GenStrMethod()
    {
        var res = "e";
        return res;
    }
        
    function GenStrMethodWithArg(s)
    {
        var res = "f" + s;
        return res;
    }

    function GenStrMethodWithArgs(s1, s2)
    {
        var res = "h" + s1 + "j" + s2;
        return res;
    }

    function main(args)
    {
        var query0 = (1 > 0) ? "true" : "false";
        var query1 = "a";
        var query2 = genString();
        var query3 = genStringWithArg("d");
        query1 += query2;
        query1 = query1 + query3;
        for (var i = 0; i < 6; i++)
        {
            query1 += GenStrMethod();
        }
        var query5 = GenStrMethodWithArg("g" + query1);
        var query6 = GenStrMethodWithArgs(query5, query1);
        query6 += withMultiReturn();
        execScript(query6);
    }
}