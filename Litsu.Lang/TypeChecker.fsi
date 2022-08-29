namespace Litsu.Lang
    
    module TypeChecker =
        
        val failCheck: unit
        
        val checkExpr: expr: SyntaxTree.Expr -> SyntaxTree.Expr
        
        val checkNode: _arg1: SyntaxTree.Node -> SyntaxTree.Node
        
        val check: p: SyntaxTree.Program -> SyntaxTree.Program

