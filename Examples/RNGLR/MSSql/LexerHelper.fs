module LexerHelper

open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection
open Yard.Examples.MSParser
open System

type Collections.Generic.IDictionary<'k,'v> with
    member d.TryGetValue' k = 
        let mutable res = Unchecked.defaultof<'v> 
        let exist = d.TryGetValue(k, &res)
        if exist then Some res else None
    member d.Add'(k,v) =
        if not (d.ContainsKey k) then d.Add(k,v);true else false

exception IdentToken

let getKwToken = 
    let nameToUnionCtor (uci:UnionCaseInfo) = (uci.Name, FSharpValue.PreComputeUnionConstructor(uci))
    let ucis = FSharpType.GetUnionCases (typeof<Token>) |> Array.map nameToUnionCtor  |> dict 
    fun (name:string) defaultSourceText ->
        let upperName = "KW_" + name.ToUpperInvariant()
        ucis.TryGetValue' upperName
        |> Option.map (fun ctor ->  ctor [| fst defaultSourceText; snd defaultSourceText |] :?>Token)

let commendepth = ref 0
let startPos = ref Position.Empty
let str_buf = new System.Text.StringBuilder()

let appendBuf (str:string) = str_buf.Append(str) |> ignore
let clearBuf () = str_buf.Clear() |> ignore
  
let makeIdent notKeyWord (name:string) (startPos, endPos) =
  let prefix = 
    if String.length name >= 2 
    then name.[0..1] 
    else ""
  let defaultSourceText = (name,(startPos,endPos))
  if prefix = "@@" then GLOBALVAR(defaultSourceText)
  //else if prefix = "##" then GLOBALTEMPOBJ(name)
  else if name.[0] = '@' then LOCALVAR(defaultSourceText)
  //else if name.[0] = '#' then TEMPOBJ(name)
  else if prefix = "%%" then STOREDPROCEDURE(defaultSourceText)
  else if notKeyWord then IDENT(defaultSourceText)
  else  match getKwToken name defaultSourceText with
        | Some(kwToken) -> kwToken
        | None -> IDENT(defaultSourceText)

let tokenPos token =
    match token with
    | KW_ABSOLUTE(_,(x,y))
    | KW_FETCH(_,(x,y))
    | KW_FETCH(_,(x,y))
    | KW_FIRST(_,(x,y))
    | KW_GLOBAL(_,(x,y))
    | KW_IS_MEMBER(_,(x,y))
    | KW_LAST(_,(x,y))
    | KW_LOCAL(_,(x,y))
    | KW_NEXT(_,(x,y))
    | KW_PRIOR(_,(x,y))
    | KW_RELATIVE(_,(x,y))
    | KW_ROLLBACK(_,(x,y))
    | KW_COMMIT (_,(x,y))
    | KW_MARK (_,(x,y))
    | KW_TRAN (_,(x,y))    
    | KW_TRANSACTION (_,(x,y))    
    | COMMA (_,(x,y))
    | DEC_NUMBER (_,(x,y))
    | DOT (_,(x,y))
    | DOUBLE_COLON (_,(x,y))
    | EMPTY (_,(x,y))
    | EOF (_,(x,y))
    | GLOBALVAR (_,(x,y))
    | IDENT (_,(x,y))
    | KW_ABSENT (_,(x,y))
    | KW_ALL (_,(x,y))
    | KW_AND (_,(x,y))
    | KW_ANSI_DEFAULTS (_,(x,y))
    | KW_ANSI_NULLS (_,(x,y))
    | KW_ANSI_NULL_DFLT (_,(x,y))
    | KW_ANSI_PADDING (_,(x,y))
    | KW_ANSI_WARNINGS (_,(x,y))
    | KW_ANY (_,(x,y))
    | KW_AS (_,(x,y))
    | KW_ASC (_,(x,y))
    | KW_AT (_,(x,y))
    | KW_AUTO (_,(x,y))
    | KW_AVG (_,(x,y))
    | KW_BASE64 (_,(x,y))
    | KW_BEGIN (_,(x,y))
    | KW_BETWEEN (_,(x,y))
    | KW_BIGINT (_,(x,y))
    | KW_BINARY (_,(x,y))
    | KW_BIT (_,(x,y))
    | KW_BROWSE (_,(x,y))
    | KW_BULK (_,(x,y))
    | KW_BY (_,(x,y))
    | KW_CALLER (_,(x,y))
    | KW_CASE (_,(x,y))
    | KW_CHAR (_,(x,y))
    | KW_CHECK (_,(x,y))
    | KW_CODEPAGE (_,(x,y))
    | KW_COLLATE (_,(x,y))
    | KW_COMPUTE (_,(x,y))
    | KW_CONCAT (_,(x,y))
    | KW_CONCAT_NULL_YIELDS_NULL (_,(x,y))
    | KW_CONTAINSTABLE (_,(x,y))
    | KW_COUNT (_,(x,y))
    | KW_CREATE (_,(x,y))
    | KW_CROSS (_,(x,y))
    | KW_CUBE (_,(x,y))
    | KW_CURSOR (_,(x,y))
    | KW_CURSOR_CLOSE_ON_COMMIT (_,(x,y))
    | KW_DECIMAL (_,(x,y))
    | KW_DECLARE (_,(x,y))
    | KW_DEFAULT (_,(x,y))
    | KW_DENSE_RANK (_,(x,y))
    | KW_DESC (_,(x,y))
    | KW_DISTINCT (_,(x,y))
    | KW_DYNAMIC (_,(x,y))
    | KW_EAD (_,(x,y))
    | KW_ELEMENTS (_,(x,y))
    | KW_ELSE (_,(x,y))
    | KW_ENCRYPTION (_,(x,y))
    | KW_END (_,(x,y))
    | KW_ERRORFILE (_,(x,y))
    | KW_ESCAPE (_,(x,y))
    | KW_EXCEPT (_,(x,y))
    | KW_EXEC (_,(x,y))
    | KW_EXECUTE (_,(x,y))
    | KW_EXECUTE_AS_Clause (_,(x,y))
    | KW_EXISTS (_,(x,y))
    | KW_EXPAND (_,(x,y))
    | KW_EXPLICIT (_,(x,y))
    | KW_FAST (_,(x,y))
    | KW_FASTFIRSTROW (_,(x,y))
    | KW_FAST_FORWARD (_,(x,y))
    | KW_FIRSTROW (_,(x,y))
    | KW_FLOAT (_,(x,y))
    | KW_FMTONLY (_,(x,y))
    | KW_FOR (_,(x,y))
    | KW_FORCE (_,(x,y))
    | KW_FORCED (_,(x,y))
    | KW_FORCEPLAN (_,(x,y))
    | KW_FORMATFILE (_,(x,y))
    | KW_FORMSOF (_,(x,y))
    | KW_FORWARD_ONLY (_,(x,y))
    | KW_FREETEXTTABLE (_,(x,y))
    | KW_FROM (_,(x,y))
    | KW_FULL (_,(x,y))
    | KW_GO (_,(x,y))
    | KW_GROUP (_,(x,y))
    | KW_HASH (_,(x,y))
    | KW_HAVING (_,(x,y))
    | KW_HOLDLOCK (_,(x,y))
    | KW_IDENTITY (_,(x,y))
    | KW_IF (_,(x,y))
    | KW_IGNORE_CONSTRAINTS (_,(x,y))
    | KW_IGNORE_TRIGGERS (_,(x,y))
    | KW_IMPLICIT_TRANSACTIONS (_,(x,y))
    | KW_IN (_,(x,y))
    | KW_INDEX (_,(x,y))
    | KW_INFLECTIONAL (_,(x,y))
    | KW_INNER (_,(x,y))
    | KW_INT (_,(x,y))
    | KW_INTERSECT (_,(x,y))
    | KW_INTO (_,(x,y))
    | KW_IO (_,(x,y))
    | KW_IS (_,(x,y))
    | KW_ISABOUT (_,(x,y))
    | KW_JOIN (_,(x,y))
    | KW_KEEP (_,(x,y))
    | KW_KEEPDEFAULTS (_,(x,y))
    | KW_KEEPFIXED (_,(x,y))
    | KW_KEEPIDENTITY (_,(x,y))
    | KW_KEY (_,(x,y))
    | KW_KEYSET (_,(x,y))
    | KW_LANGUAGE (_,(x,y))
    | KW_LASTROW (_,(x,y))
    | KW_LEFT (_,(x,y))
    | KW_LIKE (_,(x,y))
    | KW_LOG (_,(x,y))
    | KW_LOGIN (_,(x,y))
    | KW_LOOP (_,(x,y))
    | KW_LOWER (_,(x,y))
    | KW_MAX (_,(x,y))
    | KW_MAXDOP (_,(x,y))
    | KW_MAXERRORS (_,(x,y))
    | KW_MAXRECURSION (_,(x,y))
    | KW_MDW_CONTROL (_,(x,y))
    | KW_MERGE (_,(x,y))
    | KW_MIN (_,(x,y))
    | KW_MONEY (_,(x,y))
    | KW_NCHAR (_,(x,y))
    | KW_NEAR (_,(x,y))
    | KW_NOCOUNT (_,(x,y))
    | KW_NOEXEC (_,(x,y))
    | KW_NOEXPAND (_,(x,y))
    | KW_NOLOCK (_,(x,y))
    | KW_NOT (_,(x,y))
    | KW_NOWAIT (_,(x,y))
    | KW_NTILE (_,(x,y))
    | KW_NULL (_,(x,y))
    | KW_NUMERIC (_,(x,y))
    | KW_NUMERIC_ROUNDABORT (_,(x,y))
    | KW_NVARCHAR (_,(x,y))
    | KW_OF (_,(x,y))
    | KW_OFF (_,(x,y))
    | KW_ON (_,(x,y))
    | KW_OPENDATASOURCE (_,(x,y))
    | KW_OPENQUERY (_,(x,y))
    | KW_OPENROWSET (_,(x,y))
    | KW_OPTIMISTIC (_,(x,y))
    | KW_OPTIMIZE (_,(x,y))
    | KW_OPTION (_,(x,y))
    | KW_OR (_,(x,y))
    | KW_ORDER (_,(x,y))
    | KW_OUT (_,(x,y))
    | KW_OUTER (_,(x,y))
    | KW_OUTPUT (_,(x,y))
    | KW_OVER (_,(x,y))
    | KW_OWNER (_,(x,y))
    | KW_PAGLOCK (_,(x,y))
    | KW_PARAMETERIZATION (_,(x,y))
    | KW_PARSEONLY (_,(x,y))
    | KW_PARTITION (_,(x,y))
    | KW_PATH (_,(x,y))
    | KW_PERCENT (_,(x,y))
    | KW_PIVOT (_,(x,y))
    | KW_PLAN (_,(x,y))
    | KW_PRIMARY (_,(x,y))
    | KW_PROC (_,(x,y))
    | KW_PROCEDURE (_,(x,y))
    | KW_PROFILE (_,(x,y))
    | KW_QUOTED_IDENTIFIER (_,(x,y))
    | KW_RAISERROR (_,(x,y))
    | KW_RANK (_,(x,y))
    | KW_RAW (_,(x,y))
    | KW_READCOMMITTED (_,(x,y))
    | KW_READCOMMITTEDLOCK (_,(x,y))
    | KW_READONLY (_,(x,y))
    | KW_READPAST (_,(x,y))
    | KW_READUNCOMMITTED (_,(x,y))
    | KW_READ_ONLY (_,(x,y))
    | KW_REAL (_,(x,y))
    | KW_RECOMPILE (_,(x,y))
    | KW_REMOTE (_,(x,y))
    | KW_REMOTE_PROC_TRANSACTIONS (_,(x,y))
    | KW_REPEATABLE (_,(x,y))
    | KW_REPEATABLEREAD (_,(x,y))
    | KW_REPLICATION (_,(x,y))
    | KW_RETURN (_,(x,y))
    | KW_RIGHT (_,(x,y))
    | KW_ROBUST (_,(x,y))
    | KW_ROLLUP (_,(x,y))
    | KW_ROOT (_,(x,y))
    | KW_ROWGUID (_,(x,y))
    | KW_ROWGUIDCOL (_,(x,y))
    | KW_ROWLOCK (_,(x,y))
    | KW_ROWS (_,(x,y))
    | KW_ROWS_PER_BATCH (_,(x,y))
    | KW_ROW_NUMBER (_,(x,y))
    | KW_SCALAR_DATA_TYPE (_,(x,y))
    | KW_SCROLL (_,(x,y))
    | KW_SCROLL_LOCKS (_,(x,y))
    | KW_SELECT (_,(x,y))
    | KW_SELF (_,(x,y))
    | KW_SERIALIZABLE (_,(x,y))
    | KW_SET (_,(x,y))
    | KW_SETERROR (_,(x,y))
    | KW_SHOWPLAN_ALL (_,(x,y))
    | KW_SHOWPLAN_TEXT (_,(x,y))
    | KW_SHOWPLAN_XML (_,(x,y))
    | KW_SIMPLE (_,(x,y))
    | KW_SINGLE_BLOB (_,(x,y))
    | KW_SINGLE_CLOB (_,(x,y))
    | KW_SINGLE_NCLOB (_,(x,y))
    | KW_SMALLINT (_,(x,y))
    | KW_SMALLMONEY (_,(x,y))
    | KW_SOME (_,(x,y))
    | KW_SQL_VARIANT (_,(x,y))
    | KW_STATIC (_,(x,y))
    | KW_STATISTICS (_,(x,y))
    | KW_SUM (_,(x,y))
    | KW_SYSNAME (_,(x,y))
    | KW_SYSTEM (_,(x,y))
    | KW_TABLE (_,(x,y))
    | KW_TABLESAMPLE (_,(x,y))
    | KW_TABLOCK (_,(x,y))
    | KW_TABLOCKX (_,(x,y))
    | KW_THEN (_,(x,y))
    | KW_THESAURUS (_,(x,y))
    | KW_TIES (_,(x,y))
    | KW_TIME (_,(x,y))
    | KW_TINYINT (_,(x,y))
    | KW_TOP (_,(x,y))
    | KW_TYPE (_,(x,y))
    | KW_TYPE_WARNING (_,(x,y))
    | KW_UNION (_,(x,y))
    | KW_UNIQUE (_,(x,y))
    | KW_UNPIVOT (_,(x,y))
    | KW_UPDATE (_,(x,y))
    | KW_UPDLOCK (_,(x,y))
    | KW_USE (_,(x,y))
    | KW_USER (_,(x,y))
    | KW_VARBINARY (_,(x,y))
    | KW_VARCHAR (_,(x,y))
    | KW_VARYING (_,(x,y))
    | KW_VIEWS (_,(x,y))
    | KW_WHEN (_,(x,y))
    | KW_WHERE (_,(x,y))
    | KW_WITH (_,(x,y))
    | KW_XACT_ABORT (_,(x,y))
    | KW_XLOCK (_,(x,y))
    | KW_XML (_,(x,y))
    | KW_XMLDATA (_,(x,y))
    | KW_XMLSCHEMA (_,(x,y))
    | KW_XSINIL (_,(x,y))
    | LBRACKET (_,(x,y))
    | LOCALVAR (_,(x,y))
    | LPAREN (_,(x,y))
    | ONLY (_,(x,y))
    | OP_AND_EQ (_,(x,y))
    | OP_DIV (_,(x,y))
    | OP_DIV_EQ (_,(x,y))
    | OP_EQ (_,(x,y))
    | OP_GT (_,(x,y))
    | OP_LT (_,(x,y))
    | OP_MINUS (_,(x,y))
    | OP_MINUS_EQ (_,(x,y))
    | OP_MOD (_,(x,y))
    | OP_MOD_EQ (_,(x,y))
    | OP_MT (_,(x,y))
    | OP_MUL (_,(x,y))
    | OP_MUL_EQ (_,(x,y))
    | OP_OR_EQ (_,(x,y))
    | OP_PLUS (_,(x,y))
    | OP_PLUS_EQ (_,(x,y))
    | OP_TILDA (_,(x,y))
    | OP_XOR_EQ (_,(x,y))
    | PLUS (_,(x,y))
    | RBRACKET (_,(x,y))
    | RPAREN (_,(x,y))
    | SEMI (_,(x,y))
    | STAR (_,(x,y))
    | STOREDPROCEDURE (_,(x,y))
    | STRING_CONST (_,(x,y))
    | WEIGHT (_,(x,y)) -> sprintf "(%i,%i) - (%i,%i)" (x.Line+1) x.Column (y.Line+1) y.Column


let defaultSourceText (lexbuf : LexBuffer<_>) value =
    (value,(lexbuf.StartPos,lexbuf.EndPos))
          
type FsLexPosition = Microsoft.FSharp.Text.Lexing.Position

[<Struct>]
type Position =
   val line: int
   val column: int
  
   new (lin:int,col:int) = { line = lin; column = col}     
   static member inline ofLexingPosition (pos:FsLexPosition) = new Position(pos.Line, pos.Column)
   static member inline Empty = new Position (-1,-1)
   member x.toLexingPosition () =
       {  Microsoft.FSharp.Text.Lexing.pos_lnum = x.line
          Microsoft.FSharp.Text.Lexing.pos_cnum = x.column
          Microsoft.FSharp.Text.Lexing.pos_fname = ""
          Microsoft.FSharp.Text.Lexing.pos_bol =  x.AbsoluteOffset
       }
   member x.IsEmpty = x = Position.Empty
   member inline x.Line = x.line
   member inline x.Column = x.column
   member x.AbsoluteOffset = 0
   override x.ToString () = sprintf "%d,%d" x.Line x.Column
  

[<Struct>]
type SourceRange =
    val start:Position
    val end_:Position
    new (start,end_) = { start = start; end_ = end_ }
    member inline x.Start = x.start
    member inline x.End = x.end_
    static member inline Empty = new SourceRange(Position.Empty, Position.Empty)
    static member inline ofTuple(startPos:FsLexPosition, endPos:FsLexPosition) = new SourceRange(Position.ofLexingPosition startPos, Position.ofLexingPosition endPos )
    override this.ToString() =
        sprintf "(%s) - (%s)"
            (this.Start.ToString())            
            (this.End.ToString())           

[<Struct>]
type SourceText =
    val text:string
    val range:SourceRange
    new (text, range) = { text = text; range = range }
    static member inline ofTuple(text, range) = new SourceText(text,SourceRange.ofTuple range)
    static member inline ofText(text) = new SourceText(text, SourceRange.Empty)
    member inline x.Text = x.text
    member inline x.Range = x.range