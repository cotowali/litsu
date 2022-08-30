namespace Litsu
    
    module Codegen =
        
        val unreachable: string
        
        val private genNode:
          write: (string -> 'a) -> node: SyntaxTree.Node -> unit
        
        val codegen: write: (string -> 'a) -> prog: SyntaxTree.Program -> unit

