module Litsu.Testsing

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

  let testsDir = Path.Combine(settings.SolutionDirectory, "tests")

  let testFiles =
    let searchOption = SearchOption.AllDirectories
    Directory.GetFiles(testsDir, "*.lit", searchOption) |> Array.toList

  List.map
    (fun (testFilePath: string) ->
      let name = Path.GetFileName(testFilePath)
      let code = using (new StreamReader(testFilePath)) (fun r -> r.ReadToEnd())
      testTask name { do! Verifier.Verify(name, runCode code, settings, "tests") })
    testFiles
  |> testList "tests"
