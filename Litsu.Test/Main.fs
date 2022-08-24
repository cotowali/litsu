module Litsu.Test

open Expecto

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv
