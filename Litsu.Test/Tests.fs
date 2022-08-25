module Tests

open System.IO
open Expecto
open VerifyTests
open VerifyExpecto
open Litsu.Run
open Litsu.Tests

type RunResult =
  { ExitCode: int
    Stdout: string
    Stderr: string }

let runCode code : string =
  use stdin = new MemoryStream()
  use out = new MemoryStream()

  let exitCode = Runner().Run(code, stdin, out, out)

  out.Seek(0, SeekOrigin.Begin) |> ignore
  using (new StreamReader(out)) (fun r -> r.ReadToEnd())

[<Tests>]
let tests =
  let settings = Settings()
  testTask "1 + 1" { do! Verifier.Verify("add", runCode "1 + 1", settings, "tests") }
