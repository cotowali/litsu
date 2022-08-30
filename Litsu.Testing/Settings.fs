// Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Litsu.Testing.Settings

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open VerifyTests
open System.IO

type Settings([<CallerFilePath; Optional; DefaultParameterValue("")>] path: string) as this =
    inherit VerifySettings()
    let dir = Path.GetDirectoryName(path)
    let mutable solutionDirectory = dir

    do
        while Directory.GetFiles(solutionDirectory, "*.sln").Length = 0 do
            solutionDirectory <- Directory.GetParent(solutionDirectory).FullName

    do this.UseDirectory(this.OutputPath)

    member this.SolutionDirectory = solutionDirectory
    member this.OutputPath = Path.Combine(this.SolutionDirectory, "verified_output")
