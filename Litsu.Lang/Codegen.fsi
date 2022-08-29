namespace Litsu.Lang
    
    module Codegen =
        
        val unreachable: string
        
        val codegen:
          writer: System.IO.TextWriter -> node: SyntaxTree.Node -> unit

