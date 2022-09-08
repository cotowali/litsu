namespace Litsu
    
    module Type =
        
        type Type =
            | Unknown
            | String
            | Int
            | Bool
            | Var of Type option ref
        
        val newType: unit -> Type

