namespace Litsu
    
    module SyntaxTree =
        
        type Expr =
            | Int of int64
            | Add of Expr * Expr
            | Sub of Expr * Expr
        
        val typInfix: lhs: Expr -> rhs: Expr -> Type.Type
        
        val typ: _arg1: Expr -> Type.Type
        
        type Node = | Expr of Expr

