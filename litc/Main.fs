﻿// Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

open Argu
open System
open System.IO
open Litsu.Parser
open Litsu.Compiler
open Litsu.Run

type MainArgs =
    | [<MainCommand>] File of file: string
    | [<CliPrefix(CliPrefix.None)>] Run
    | [<CliPrefix(CliPrefix.None)>] Ast

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | File _ -> "input file"
            | Run _ -> "run after compile"
            | Ast _ -> "print ast"

[<EntryPoint>]
let main argv =
    let p =
        ArgumentParser.Create<MainArgs>(
            programName = "litc",
            errorHandler =
                ProcessExiter(
                    colorizer =
                        function
                        | ErrorCode.HelpText -> None
                        | _ -> Some ConsoleColor.Red
                )
        )

    let results = p.ParseCommandLine argv

    let code =
        use file =
            (match results.GetResults(MainArgs.File) with
             | [] ->
                 (if Console.IsInputRedirected then
                      Console.In
                  else
                      p.PrintUsage() |> printf "%s"
                      exit 0)
             | [ path ] -> new StreamReader(path)
             | _ -> eprintfn "too many input" |> exit 1)

        file.ReadToEnd()

    let isRun = results.TryGetResult(MainArgs.Run) <> None
    let isAst = results.TryGetResult(MainArgs.Ast) <> None

    if isAst then
        parse code |> printfn "%A"
        0
    else if isRun then
        run code
    else
        compile code |> printf "%s"
        0
