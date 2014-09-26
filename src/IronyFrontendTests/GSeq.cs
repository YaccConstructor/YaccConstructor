using System;
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

    public class TermName : Grammar
    {
      public TermName()
      {
        this.GrammarComments = @"Test TermName grammar";
        //Terminals
        KeyTerm greater = ToTerm(">");
        KeyTerm less = ToTerm("<");
        KeyTerm equal = ToTerm("=");

        var s = new NonTerminal("Start");

        //Rules
        s.Rule = greater | less | equal;

        this.Root = s;
        this.LanguageFlags |= LanguageFlags.NewLineBeforeEOF;

      }//constructor

    }
}
