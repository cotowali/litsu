namespace Litsu
    
    module Type =
        
        type Type =
            | Unknown
            | String
            | Int
            | Bool
            | Fun of Type list * Type
            | Var of Type option ref
        
        val newType: unit -> Type

