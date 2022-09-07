(***********************************************************************)
(*                                                                     *)
(* Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.        *)
(*                                                                     *)
(* This Source Code Form is subject to the terms of the Mozilla Public *)
(* License, v. 2.0. If a copy of the MPL was not distributed with this *)
(* file, You can obtain one at https://mozilla.org/MPL/2.0/.           *)
(*                                                                     *)
(***********************************************************************)

module Litsu.TypeEnv

open Litsu.Type

type TypeEnv = Map<string, Type>

let add: (string -> Type -> TypeEnv -> TypeEnv) = Map.add
let exists: string -> TypeEnv -> bool = Map.containsKey
let find: (string -> TypeEnv -> Type) = Map.find
let tryFind: (string -> TypeEnv -> Type option) = Map.tryFind
