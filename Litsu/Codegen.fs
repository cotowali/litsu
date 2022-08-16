// Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Litsu.Codegen

open System.IO
open Litsu.SyntaxTree

let codegen (writer: TextWriter) (node: Node) : unit =
  match node with
  | Expr (expr) ->
    let rec f: Expr -> string =
      function
      | Int (n) -> sprintf "%d" n
      | Add (lhs, rhs) -> sprintf "$(( %s + %s ))" (f lhs) (f rhs)

    writer.Write($"printf '%%s\\n' \"{f expr}\"")
