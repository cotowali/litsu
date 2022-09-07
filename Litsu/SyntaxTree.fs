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
    | Add of Expr * Expr * Type
    | Sub of Expr * Expr * Type
    | Eq of Expr * Expr
    | Let of string * Type * Expr * Expr
    | Var of string * Type

let rec typ: (Expr -> Type) =
    function
    | Expr.Int (_) -> Type.Int
    | Expr.Add (_, _, t)
    | Expr.Sub (_, _, t) -> t
    | Expr.Eq (_) -> Type.Bool
    | Expr.Let (_, t, _, _) -> t
    | Expr.Var (_, t) -> t

type Node = Expr of Expr
type Program = { Nodes: Node list }
