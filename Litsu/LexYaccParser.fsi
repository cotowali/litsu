namespace Litsu
    
    module internal LexYaccParser =
        
        val parse_error_rich:
          (FSharp.Text.Parsing.ParseErrorContext<'a> -> 'b) option
        
        type token =
            | EQ
            | PLUS
            | MINUS
            | LET
            | IN
            | IDENT of string
            | STRING of string
            | INT of int64
            | EOF
        
        type tokenId =
            | TOKEN_EQ
            | TOKEN_PLUS
            | TOKEN_MINUS
            | TOKEN_LET
            | TOKEN_IN
            | TOKEN_IDENT
            | TOKEN_STRING
            | TOKEN_INT
            | TOKEN_EOF
            | TOKEN_end_of_input
            | TOKEN_error
        
        type nonTerminalId =
            | NONTERM__startprogram
            | NONTERM_program
            | NONTERM_nodes
            | NONTERM_rev_nodes
            | NONTERM_node
            | NONTERM_expr
        
        val tagOfToken: t: token -> int
        
        val tokenTagToTokenId: tokenIdx: int -> tokenId
        
        /// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
        val prodIdxToNonTerminal: prodIdx: int -> nonTerminalId
        
        val _fsyacc_endOfInputTag: int
        
        val _fsyacc_tagOfErrorTerminal: int
        
        val token_to_string: t: token -> string
        
        val _fsyacc_dataOfToken: t: token -> System.Object
        
        val _fsyacc_gotos: uint16[]
        
        val _fsyacc_sparseGotoTableRowOffsets: uint16[]
        
        val _fsyacc_stateToProdIdxsTableElements: uint16[]
        
        val _fsyacc_stateToProdIdxsTableRowOffsets: uint16[]
        
        val _fsyacc_action_rows: int
        
        val _fsyacc_actionTableElements: uint16[]
        
        val _fsyacc_actionTableRowOffsets: uint16[]
        
        val _fsyacc_reductionSymbolCounts: uint16[]
        
        val _fsyacc_productionToNonTerminalTable: uint16[]
        
        val _fsyacc_immediateActions: uint16[]
        
        val _fsyacc_reductions:
          unit -> (FSharp.Text.Parsing.IParseState -> obj)[]
        
        val tables: FSharp.Text.Parsing.Tables<token>
        
        val engine:
          lexer: (FSharp.Text.Lexing.LexBuffer<'a> -> token)
          -> lexbuf: FSharp.Text.Lexing.LexBuffer<'a> -> startState: int -> obj
        
        val program:
          lexer: (FSharp.Text.Lexing.LexBuffer<'a> -> token)
          -> lexbuf: FSharp.Text.Lexing.LexBuffer<'a> -> SyntaxTree.Program

