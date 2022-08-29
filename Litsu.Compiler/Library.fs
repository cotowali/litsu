// Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Litsu.Compiler

open System.IO
open System.Text
open Litsu.Parser
open Litsu.TypeChecker
open Litsu.Codegen

let compile code =
  let out = StringBuilder()
  parse code |> check |> codegen (new StringWriter(out))
  out.ToString()
