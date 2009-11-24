namespace Yard.Core

type ParserResult<'symb,'leafVal,'nodeVal when 'symb : equality and 'symb : comparison> = struct
 val position : int;
 val state : State<'symb,'leafVal,'nodeVal>; 
 new (state,position) = {state=state; position=position}
end