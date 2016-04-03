using System;
using System.Collections.Generic;
using System.Linq;

using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.ReSharper.Resources.Shell;
using JetBrains.Util;

using Microsoft.FSharp.Control;

using YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation;
using YC.SDK;


namespace ReSharperExtension.YcIntegration
{
    using YcLexEvent = IEvent
        <
            FSharpHandler<CommonInterfaces.LexingFinishedArgs<ITreeNode>>,
            CommonInterfaces.LexingFinishedArgs<ITreeNode>
        >;
    using YcParseEvent = IEvent
        <
            FSharpHandler<CommonInterfaces.ParsingFinishedArgs>, 
            CommonInterfaces.ParsingFinishedArgs
        >;

    class ReSharperHelper<TRange, TNode> 
        where TNode : ITreeNode
    {
        private static ReSharperHelper<TRange, TNode> instance;
        public static ReSharperHelper<TRange, TNode> Instance
        {
            get
            {
                if (instance == null)
                    instance = new ReSharperHelper<TRange, TNode>();
                return instance;
            }
        }

        private List<YcLexEvent> lexerEvents;
        public List<YcLexEvent> LexingFinished
        {
            get
            {
                if (lexerEvents == null)
                {
                    IEnumerable<IReSharperLanguage> processors = GetAllProcessors();
                    lexerEvents = new List<YcLexEvent>();
                    foreach (IReSharperLanguage proc in processors)
                        lexerEvents.Add(proc.LexingFinished);
                }
                return lexerEvents;
            }
        }

        private List<YcParseEvent> parserEvents;
        public List<YcParseEvent> ParsingFinished
        {
            get
            {
                if (parserEvents == null)
                {
                    parserEvents = new List<YcParseEvent>();

                    foreach (IReSharperLanguage processor in GetAllProcessors())
                        parserEvents.Add(processor.ParsingFinished);
                }
                return parserEvents;
            }
        }
      
        private ReSharperHelper()
        {
        }

        public List<DocumentRange> GetPairedRanges(string lang, int left, int right, DocumentRange range, bool toRight)
        {
            return GetProcessor(lang).GetPairedRanges(left, right, range, toRight);
        }

//        public List<ITreeNode> GetForestWithToken(string lang, DocumentRange range)
//        {
//            return GetProcessor(lang).GetForestWithToken(range);
//        }
//
//        public Tuple<ITreeNode, bool> GetNextTree(string lang, int number)
//        {
//            return GetProcessor(lang).GetNextTree(number);
//        }

        public IEnumerable<string> GetAvailableTokens(string lang)
        {
            return GetProcessor(lang).TokenNames;
        }

        public ProcessErrors Process(IFile file)
        {
            var lexerErrors = new List<ErrorInfo>();
            var parserErrors = new List<ErrorInfo>();
            var semanticErrors = new List<ErrorInfo>();

            var graphs = (new Approximator(file)).Approximate(Handler.Hotspots);

            foreach (var tuple in graphs)
            {
                string lang = tuple.Item1;
                var graph = tuple.Item2;

                var triple = GetProcessor(lang).Process(graph);
                List<Tuple<string, DocumentRange>> newLexerErrors = triple.Item1;
                List<Tuple<string, DocumentRange>> newParserErrors = triple.Item2;
                List<Tuple<string, DocumentRange>> newSemanticErrors = triple.Item3;

                lexerErrors.AddRange(newLexerErrors.Select(pair => TupleToErrorInfo(pair)));
                parserErrors.AddRange(newParserErrors.Select(pair => TupleToErrorInfo(pair)));
                semanticErrors.AddRange(newSemanticErrors.Select(pair => TupleToErrorInfo(pair)));
            }

            return new ProcessErrors(lexerErrors, parserErrors, semanticErrors);
        }

        private IEnumerable<IReSharperLanguage> GetAllProcessors()
        {
            return Shell.Instance.GetComponents<IReSharperLanguage>();
        }

        public IEnumerable<string> GetAllLanguagesNames()
        {
            return GetAllProcessors().Select(processor => processor.Name);
        }

        private IReSharperLanguage GetProcessor(string lang)
        {
            List<IReSharperLanguage> processors =  GetAllProcessors().AsList();
            lang = lang.ToLowerInvariant();

            IReSharperLanguage res =  processors.Find(processor => processor.Name.ToLowerInvariant() == lang);
            if (res == null)
                throw new Exception(String.Format("{0} language isn't found. Total loaded: {1}", lang, processors.Count));

            return res;
        }

        private ErrorInfo TupleToErrorInfo(Tuple<string, DocumentRange> pair)
        {
            return new ErrorInfo
                    {
                        Message = pair.Item1,
                        Range = pair.Item2,
                    };
        }
    }
}
