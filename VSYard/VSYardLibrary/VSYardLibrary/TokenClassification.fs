namespace VSYardNS

open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Editor
open Yard.Frontends.YardFrontend.GrammarParser

type TokenTypes =
    | TLiteral
    | TTerm
    | TNonterm
    | TComment
    | TOther

type TokenTag (_type: TokenTypes) =
    let mutable lType = TOther
    do lType <- _type
    interface ITag
    member self.Type
        with get () = _type

type TokenTagger (buffer:ITextBuffer) =
    let mutable _buffer = null
    let mutable _ookTypes = null
    do
        _buffer <- buffer;            
        
    let getTags (spans: NormalizedSnapshotSpanCollection) =        
        spans
        |> Seq.map 
            (fun curSpan ->            
                let containingLine = curSpan.Start.GetContainingLine()
                let tokens = Yard.Frontends.YardFrontend.Main.LexString (curSpan.GetText())
                let curLoc = ref containingLine.Start.Position;
                tokens
                |> Seq.choose
                    (fun t ->                        
                        let l = ref 0
                        let pos (_,(x,_)) = x                            
                        let tokenSpan _l = 
                            l := _l
                            new SnapshotSpan(curSpan.Snapshot, new Span(!curLoc, _l))
                        let res =                                                        
                            let r = 
                                let length x = Yard.Core.IL.Source.toString x |> String.length
                                match t with
                                | UIDENT x -> new TagSpan<TokenTag>(length x |> tokenSpan , new TokenTag(TTerm))
                                | LIDENT x -> new TagSpan<TokenTag>(length x |> tokenSpan , new TokenTag(TNonterm))
                                | STRING x -> new TagSpan<TokenTag>(length x + 2 |> tokenSpan , new TokenTag(TLiteral))
                                | PATTERN x -> new TagSpan<TokenTag>(length x|> tokenSpan , new TokenTag(TLiteral))
                                | ACTION x -> new TagSpan<TokenTag>(length x + 2|> tokenSpan , new TokenTag(TLiteral))
                                | SEMICOLON
                                | COLON
                                | PLUS
                                | MINUS
                                | QUESTION
                                | EQUAL
                                | LPAREN
                                | RPAREN
                                | BAR                               
                                | STAR  -> TagSpan<TokenTag>(tokenSpan 1, new TokenTag(TOther))
                                | DLESS  
                                | DGREAT -> TagSpan<TokenTag>(tokenSpan 2, new TokenTag(TOther))
                                | _ -> new TagSpan<TokenTag>(tokenSpan 1, new TokenTag(TOther)) 
                            if ((tokenSpan !l).IntersectsWith(curSpan))
                            then Some r        
                            else None
                        curLoc := !curLoc + !l + 1
                        res 
                    )
                (*let curLoc = ref containingLine.Start.Position;
                let tokens = containingLine.GetText().Split(' ');

                tokens
                |> Seq.choose 
                    (fun ookToken ->                
                        let tokenSpan = new SnapshotSpan(curSpan.Snapshot, new Span(!curLoc, ookToken.Length))
                        let res =
                             
                            if (tokenSpan.IntersectsWith(curSpan) && ookToken.ToUpper() = ookToken)
                            then new TagSpan<TokenTag>(tokenSpan, new TokenTag(TTerm)) |> Some
                            else new TagSpan<TokenTag>(tokenSpan, new TokenTag(TNonterm)) |> Some
                        curLoc := !curLoc + ookToken.Length + 1
                        res             
                   )*)
            )
            |> Seq.concat

    interface  ITagger<TokenTag> with
        member self.GetTags spans = getTags spans :?> _
        member self.add_TagsChanged x = () 
        member self.remove_TagsChanged x = () 
    
    (*public event EventHandler<SnapshotSpanEventArgs> TagsChanged
    {
        add { }
        remove { }
    }*)

    (*public IEnumerable<ITagSpan<TokenTag>> GetTags(NormalizedSnapshotSpanCollection spans)
    {

        

    }
}*)


