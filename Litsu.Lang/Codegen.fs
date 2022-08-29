// Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Litsu.Lang.Codegen

open System.IO
open Litsu.Lang.SyntaxTree
open Litsu.Lang.Type

let unreachable = failwith "Unreachable"

let private genNode (writer: TextWriter) (node: Node) : unit =
  match node with
  | Expr (expr) ->
    let rec f: Expr -> string =
      function
      | Expr.Int (n) -> sprintf "%d" n
      | Add (lhs, rhs) ->
        match typInfix lhs rhs with
        | Type.Int -> sprintf "$(( %s + %s ))" (f lhs) (f rhs)
        | _ -> unreachable
      | Sub (lhs, rhs) ->
        match typInfix lhs rhs with
        | Type.Int -> sprintf "$(( %s - %s ))" (f lhs) (f rhs)
        | _ -> unreachable
      | Eq (lhs, rhs) ->
        let op =
          match typ lhs with
          | Type.Int -> "-eq"
          | Type.Bool -> "="
          | _ -> unreachable in

        let cond = sprintf "[ \"%s\" %s \"%s\" ]" (f lhs) op (f rhs) in
        sprintf "$(%s && printf 'true' || printf 'false')" cond

    writer.Write($"printf '%%s\\n' \"{f expr}\"\n")

let codegen (writer: TextWriter) (prog: Program) : unit =
  List.map (genNode writer) prog.Nodes |> ignore
