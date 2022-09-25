namespace Litsu
    
    module Codegen =
        
        val unreachable: unit -> 'a
        
        type Context =
            {
              IndentN: int
              Counter: int ref
            }
        
        val private newContext: unit -> Context
        
        val private newInnerContext: ctx: Context -> Context
        
        val private varname: name: string -> string
        
        val private sTrue: string
        
        val sFalse: string
        
        val private indentStr: n: int -> string
        
        val private ctxIndentStr: ctx: Context -> string
        
        type writeF = string -> unit
        
        val private genExpr:
          ctx: Context -> write: writeF -> expr: SyntaxTree.Expr -> unit
        
        val private genNode:
          ctx: Context -> write: (string -> unit) -> node: SyntaxTree.Node
            -> unit
        
        val codegen: write: (string -> unit) -> prog: SyntaxTree.Program -> unit

