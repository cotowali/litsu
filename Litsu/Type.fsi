namespace Litsu
    
    module Type =
        
        type Type =
            | Unknown
            | Int
            | Bool
            | Var of Type option ref

