// Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Litsu.Lang.SyntaxTree

open Litsu.Lang.Type

type Expr =
  | Int of int64
  | Add of Expr * Expr
  | Sub of Expr * Expr

let rec typInfix (lhs: Expr) (rhs: Expr) =
  let lType = typ (lhs)
  let rType = typ (rhs)
  if lType = rType then lType else Type.Unknown

and typ: (Expr -> Type) =
  function
  | Expr.Int (_) -> Type.Int
  | Expr.Add (lhs, rhs) -> typInfix lhs rhs
  | Expr.Sub (lhs, rhs) -> typInfix lhs rhs

type Node = Expr of Expr
