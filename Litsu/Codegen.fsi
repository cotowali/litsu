namespace Litsu
    
    module Codegen =
        
        val unreachable: string
        
        val private genNode:
          writer: System.IO.TextWriter -> node: SyntaxTree.Node -> unit
        
        val codegen:
          writer: System.IO.TextWriter -> prog: SyntaxTree.Program -> unit

