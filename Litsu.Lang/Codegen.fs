// Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Litsu.Lang.Codegen

open System.IO
open Litsu.Lang.SyntaxTree
open Litsu.Lang.Type

let failwithUnknownType = failwith "Unknown Type"

let codegen (writer: TextWriter) (node: Node) : unit =
  match node with
  | Expr (expr) ->
    let rec f: Expr -> string =
      function
      | Expr.Int (n) -> sprintf "%d" n
      | Add (lhs, rhs) ->
        match typInfix lhs rhs with
        | Type.Int -> sprintf "$(( %s + %s ))" (f lhs) (f rhs)
        | Type.Unknown -> failwithUnknownType
      | Sub (lhs, rhs) ->
        match typInfix lhs rhs with
        | Type.Int -> sprintf "$(( %s - %s ))" (f lhs) (f rhs)
        | Type.Unknown -> failwithUnknownType

    writer.Write($"printf '%%s\\n' \"{f expr}\"")
