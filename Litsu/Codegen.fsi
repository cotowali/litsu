namespace Litsu
    
    module Codegen =
        
        val unreachable: unit -> 'a
        
        type Context =
            { IndentN: int }
        
        val private newContext: unit -> Context
        
        val private varname: name: string -> string
        
        val private genExpr:
          ctx: Context -> write: (string -> unit) -> expr: SyntaxTree.Expr
            -> unit
        
        val private genNode:
          ctx: Context -> write: (string -> unit) -> node: SyntaxTree.Node
            -> unit
        
        val codegen: write: (string -> unit) -> prog: SyntaxTree.Program -> unit

