namespace Litsu
    
    module SyntaxTree =
        
        type Expr =
            | Int of int64
            | Add of Expr * Expr
        
        type Node = | Expr of Expr

