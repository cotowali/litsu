// Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Litsu.Run

open System
open System.IO
open System.Text
open System.Diagnostics
open CliWrap
open Litsu.Compiler
open Litsu.Parser
open Litsu.Codegen

type Runner() =
  member _this.Run(code: string, ?stdin: Stream, ?stdout: Stream, ?stderr: Stream) : int =

    let stdin = defaultArg stdin (Console.OpenStandardInput())
    let stdout = defaultArg stdout (Console.OpenStandardOutput())
    let stderr = defaultArg stderr (Console.OpenStandardError())

    let filename = $"litsu_{DateTime.UtcNow.Ticks}.sh"
    let outPath = Path.Join(Path.GetTempPath(), filename)

    use file =
      new StreamWriter(
        outPath,
        UTF8Encoding(false),
        FileStreamOptions(Access = FileAccess.ReadWrite, Mode = FileMode.CreateNew, Options = FileOptions.DeleteOnClose)
      )

    compile code |> file.Write
    file.Flush()

    let cmd = Cli.Wrap("sh").WithArguments(outPath)
    let cmd = cmd.WithValidation(CommandResultValidation.None)
    let stdinPipe = PipeSource.FromStream(stdin)
    let cmd = cmd.WithStandardInputPipe(stdinPipe)
    let stdoutPipe = PipeTarget.ToStream(stdout)
    let cmd = cmd.WithStandardOutputPipe(stdoutPipe)
    let stderrPipe = PipeTarget.ToStream(stderr)
    let cmd = cmd.WithStandardErrorPipe(stderrPipe)

    let res = (cmd.ExecuteAsync().Task.Result)

    res.ExitCode

let run code = Runner().Run code
