namespace Litsu
    
    module TypeEnv =
        
        type TypeEnv = Map<string,Type.Type>
        
        val add: (string -> Type.Type -> TypeEnv -> TypeEnv)
        
        val addList: items: (string * Type.Type) list -> env: TypeEnv -> TypeEnv
        
        val exists: (string -> TypeEnv -> bool)
        
        val find: (string -> TypeEnv -> Type.Type)
        
        val tryFind: (string -> TypeEnv -> Type.Type option)

