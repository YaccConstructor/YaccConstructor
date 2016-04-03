using System.Collections.Generic;

using JetBrains.DocumentModel;

namespace ReSharperExtension.YcIntegration
{
    class ErrorInfo
    {
        public string Message { get; set; }
        public DocumentRange Range { get; set; }
    }

    class ProcessErrors
    {
        public List<ErrorInfo> LexerErrors { get; private set; }
        public List<ErrorInfo> ParserErrors { get; private set; }
        public List<ErrorInfo> SemanticErrors { get; private set; }

        public ProcessErrors(List<ErrorInfo> lexerErrors, List<ErrorInfo> parserErrors, List<ErrorInfo> semanticErrors)
        {
            LexerErrors = lexerErrors;
            ParserErrors = parserErrors;
            SemanticErrors = semanticErrors;
        }

        public List<ErrorInfo> GetAllErrors()
        {
            var result = new List<ErrorInfo>();
            result.AddRange(LexerErrors);
            result.AddRange(ParserErrors);
            result.AddRange(SemanticErrors);
            return result;
        }
    }
}
