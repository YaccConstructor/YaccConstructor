using System;
using Irony.Parsing;


namespace IronyFrontendTests
{
    public class NTermName : Grammar
    {
        public NTermName()
        {
            this.GrammarComments = @"Test Seq grammar";
            //Terminals
            var term1 = new DsvLiteral("<", TypeCode.String);

            var MyNonTerm = new NonTerminal("MyNonTerm");

            //Rules
            MyNonTerm.Rule = term1;
            this.Root = MyNonTerm;
            this.LanguageFlags |= LanguageFlags.NewLineBeforeEOF;            

        }//constructor
    }
}
