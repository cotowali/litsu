module Tests

open System.IO
open Expecto
open Litsu.Run

type RunResult =
  { ExitCode: int
    Stdout: string
    Stderr: string }

let runCode code : RunResult =
  use stdin = new MemoryStream()
  use stdout = new MemoryStream()
  use stderr = new MemoryStream()

  let exitCode = Runner().Run(code, stdin, stdout, stderr)

  stdout.Seek(0, SeekOrigin.Begin) |> ignore
  stderr.Seek(0, SeekOrigin.Begin) |> ignore
  use stdoutReader = new StreamReader(stdout)
  use stderrReader = new StreamReader(stderr)

  { ExitCode = exitCode
    Stdout = stdoutReader.ReadToEnd()
    Stderr = stderrReader.ReadToEnd() }

[<Tests>]
let tests =
  testList
    "valid codes"
    [ test "1 + 1" {
        Expect.equal
          (runCode "1 + 1")
          { ExitCode = 0
            Stdout = "2\n"
            Stderr = "" }
          "1 + 1 = 2"
      } ]
