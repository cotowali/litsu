// Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Litsu.Tests

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open VerifyTests
open System.IO

type Settings([<CallerFilePath; Optional>] path: string) as this =
  inherit VerifySettings()
  do this.UseDirectory(this.GetOutputDirectory())

  member private _.GetOutputDirectory([<CallerFilePath; Optional; DefaultParameterValue("")>] path: string) : string =
    Path.Combine(Path.GetDirectoryName(path), "output")
