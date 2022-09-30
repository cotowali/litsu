namespace Litsu
    
    module SyntaxTree =
        
        type Expr =
            | Int of int64
            | String of string
            | Unit
            | Infix of Infix
            | Let of Let
            | App of App
            | Var of Var
            | If of If
        
        and Infix =
            {
              Op: string
              Left: Expr
              Right: Expr
              Type: Type.Type
            }
        
        and Let =
            {
              Name: string
              Type: Type.Type
              Args: Var list
              IsRec: bool
              Expr1: Expr
              Expr2: Expr
            }
        
        and App =
            {
              Fun: Expr
              Args: Expr list
              Type: Type.Type
            }
        
        and Var =
            {
              Name: string
              Type: Type.Type
            }
        
        and If =
            {
              Cond: Expr
              Expr1: Expr
              Expr2: Expr
              Type: Type.Type
            }
        
        val typ: _arg1: Expr -> Type.Type
        
        type Node = | Expr of Expr
        
        type Program =
            { Nodes: Node list }
        
        exception SyntaxError of FSharp.Text.Lexing.Position * string option
        
        exception ExprException of Expr * string option

