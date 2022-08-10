// Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Litsu

open FParsec

let parse text =
  match run pint64 text with
  | Success (res, _, _) -> sprintf "%d" res
  | Failure (msg, _, _) -> failwithf "parse error: %s" msg