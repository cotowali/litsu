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
    | String of string
    | Infix of string * Expr * Expr * Type // op lhs rhs typ
    | Let of string * Type * Expr * Expr
    | Var of string * Type
    | If of Expr * Expr * Expr * Type // cond trueBody falseBody typ

let rec typ: (Expr -> Type) =
    function
    | Expr.Int (_) -> Type.Int
    | Expr.String (_) -> Type.String
    | Expr.Infix (_, _, _, t) -> t
    | Expr.If (_, _, _, t) -> t
    | Expr.Let (_, t, _, _) -> t
    | Expr.Var (_, t) -> t

type Node = Expr of Expr
type Program = { Nodes: Node list }
