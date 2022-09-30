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

open FSharp.Text.Lexing
open Litsu.Type

type Expr =
    | Int of int64
    | String of string
    | Unit
    | Infix of Infix
    | Let of Let
    | App of App
    | Var of Var
    | If of If

and Infix =
    { Op: string
      Left: Expr
      Right: Expr
      Type: Type }

and Let =
    { Name: string
      Type: Type
      Args: Var list
      IsRec: bool
      Expr1: Expr
      Expr2: Expr }

and App =
    { Fun: Expr
      Args: Expr list
      Type: Type }

and Var = { Name: string; Type: Type }

and If =
    { Cond: Expr
      Expr1: Expr
      Expr2: Expr
      Type: Type }

let rec typ: (Expr -> Type) =
    function
    | Expr.Int (_) -> Type.Int
    | Expr.String (_) -> Type.String
    | Expr.Unit -> Type.Unit
    | Expr.Infix ({ Type = t })
    | Expr.If ({ Type = t })
    | Expr.Let ({ Type = t })
    | Expr.App ({ Type = t })
    | Expr.Var ({ Type = t }) -> t

type Node = Expr of Expr
type Program = { Nodes: Node list }

exception SyntaxError of Position * string option
exception ExprException of Expr * string option
