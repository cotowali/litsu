namespace Litsu
    
    module SyntaxTree =
        
        type Expr =
            | Int of int64
            | Add of Expr * Expr * Type.Type
            | Sub of Expr * Expr * Type.Type
            | Eq of Expr * Expr
            | Let of string * Type.Type * Expr * Expr
            | Var of string * Type.Type
        
        val typ: _arg1: Expr -> Type.Type
        
        type Node = | Expr of Expr
        
        type Program =
            { Nodes: Node list }

