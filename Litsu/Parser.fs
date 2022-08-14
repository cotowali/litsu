// Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Litsu.Parser

open FSharp.Text.Lexing
open Litsu.SyntaxTree

let parse text: SyntaxTree.Node =
  let lexbuf = LexBuffer<char>.FromString text
  LexYaccParser.program LexYaccLexer.read lexbuf
