namespace Litsu
    
    module internal LexYaccLexer =
        
        val lexeme: arg00: FSharp.Text.Lexing.LexBuffer<char> -> string
        
        val newline: lexbuf: FSharp.Text.Lexing.LexBuffer<'a> -> unit
        
        val trans: uint16[] array
        
        val actions: uint16[]
        
        val _fslex_tables: FSharp.Text.Lexing.UnicodeTables
        
        val _fslex_dummy: unit -> 'a
        
        val read:
          lexbuf: FSharp.Text.Lexing.LexBuffer<char> -> LexYaccParser.token

