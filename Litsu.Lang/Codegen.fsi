namespace Litsu.Lang
    
    module Codegen =
        
        val failwithUnknownType: string
        
        val codegen:
          writer: System.IO.TextWriter -> node: SyntaxTree.Node -> unit

