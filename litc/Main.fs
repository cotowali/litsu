// Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

open Litsu.Compiler
open Litsu.Run

"1 + 2 - 3 + 4 - 5" |> compile |> printfn "%s"
run "1 + 2 - 3 + 4 - 5" |> printfn "%d"
