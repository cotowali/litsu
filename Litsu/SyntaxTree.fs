(***********************************************************************)
(*                                                                     *)
(* Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.        *)
(*                                                                     *)
(* This Source Code Form is subject to the terms of the Mozilla Public *)
(* License, v. 2.0. If a copy of the MPL was not distributed with this *)
(* file, You can obtain one at https://mozilla.org/MPL/2.0/.           *)
(*                                                                     *)
(***********************************************************************)

module Litsu.SyntaxTree

open Litsu.Type

type Expr =
    | Int of int64
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Eq of Expr * Expr

let rec typ: (Expr -> Type) =
    function
    | Expr.Int (_) -> Type.Int
    | Expr.Add (lhs, rhs) -> typInfix lhs rhs
    | Expr.Sub (lhs, rhs) -> typInfix lhs rhs
    | Expr.Eq (_) -> Type.Bool

and typInfix (lhs: Expr) (rhs: Expr) =
    let lType = typ (lhs) in
    let rType = typ (rhs) in
    if lType = rType then lType else Type.Unknown

type Node = Expr of Expr
type Program = { Nodes: Node list }
