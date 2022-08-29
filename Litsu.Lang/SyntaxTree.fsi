namespace Litsu.Lang
    
    module SyntaxTree =
        
        type Expr =
            | Int of int64
            | Add of Expr * Expr
            | Sub of Expr * Expr
            | Eq of Expr * Expr
        
        val typ: _arg1: Expr -> Type.Type
        
        val typInfix: lhs: Expr -> rhs: Expr -> Type.Type
        
        type Node = | Expr of Expr

