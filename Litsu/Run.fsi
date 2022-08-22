namespace Litsu
    
    module Run =
        
        type Runner =
            
            new: unit -> Runner
            
            member
              Run: code: string * ?stdin: System.IO.Stream *
                   ?stdout: System.IO.Stream * ?stderr: System.IO.Stream -> int
        
        val run: code: string -> int

