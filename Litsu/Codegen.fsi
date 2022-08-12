namespace Litsu
    
    module Codegen =
        
        val codegen:
          writer: System.IO.TextWriter -> node: SyntaxTree.Node -> unit

