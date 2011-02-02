using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Irony.Parsing;

namespace IronyFrontendTests
{
    public class GSeq : Grammar
    {
        public GSeq()
        {
            this.GrammarComments = @"Test Seq grammar";
            //Terminals
            var term1 = new DsvLiteral("MULT", TypeCode.String);
            var term2 = new DsvLiteral("PLUS", TypeCode.String);

            var s = new NonTerminal("S");

            //Rules
            s.Rule = term1 + term2;
            this.Root = s;
            this.LanguageFlags |= LanguageFlags.NewLineBeforeEOF;

        }//constructor

    }
}
