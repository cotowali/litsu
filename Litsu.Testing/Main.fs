module Litsu.Testing

open Expecto

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv
