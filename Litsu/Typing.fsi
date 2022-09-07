namespace Litsu
    
    module Typing =
        
        exception UnifyException of Type.Type * Type.Type
        
        val occur: r1: Type.Type option ref -> _arg1: Type.Type -> bool
        
        val unify: t1: Type.Type -> t2: Type.Type -> unit
        
        val derefType: _arg1: Type.Type -> Type.Type
        
        val derefExpr: expr: SyntaxTree.Expr -> SyntaxTree.Expr
        
        val derefNode: _arg1: SyntaxTree.Node -> SyntaxTree.Node
        
        val deref: p: SyntaxTree.Program -> SyntaxTree.Program
        
        val infer: env: TypeEnv.TypeEnv -> e: SyntaxTree.Expr -> Type.Type
        
        val check: p: SyntaxTree.Program -> SyntaxTree.Program

