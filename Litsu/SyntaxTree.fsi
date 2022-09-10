namespace Litsu
    
    module SyntaxTree =
        
        type Expr =
            | Int of int64
            | String of string
            | Infix of string * Expr * Expr * Type.Type
            | Let of string * Type.Type * Expr * Expr
            | Var of string * Type.Type
            | If of Expr * Expr * Expr * Type.Type
        
        val typ: _arg1: Expr -> Type.Type
        
        type Node = | Expr of Expr
        
        type Program =
            { Nodes: Node list }

