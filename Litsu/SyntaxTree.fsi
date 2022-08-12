namespace Litsu
    
    module SyntaxTree =
        
        type Expr = | Int of int64
        
        type Node = | Expr of Expr

