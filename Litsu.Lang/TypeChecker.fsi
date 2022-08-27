namespace Litsu.Lang
    
    module TypeChecker =
        
        val failCheck: unit
        
        val checkExpr: expr: SyntaxTree.Expr -> SyntaxTree.Expr
        
        val check: _arg1: SyntaxTree.Node -> SyntaxTree.Node

