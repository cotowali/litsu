(***********************************************************************)
(*                                                                     *)
(* Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.        *)
(*                                                                     *)
(* This Source Code Form is subject to the terms of the Mozilla Public *)
(* License, v. 2.0. If a copy of the MPL was not distributed with this *)
(* file, You can obtain one at https://mozilla.org/MPL/2.0/.           *)
(*                                                                     *)
(***********************************************************************)

module Litsu.TypeChecker

open Litsu.Type
open Litsu.SyntaxTree

let failCheck = failwith "Type Error"

let rec checkExpr (expr: Expr) : Expr =
    let expr =
        match expr with
        | Expr.Int (n) -> Expr.Int(n)
        | Expr.Add (lhs, rhs) ->
            let lhs = checkExpr (lhs) in
            let rhs = checkExpr (rhs) in
            Expr.Add(lhs, rhs)
        | Expr.Sub (lhs, rhs) ->
            let lhs = checkExpr (lhs) in
            let rhs = checkExpr (rhs) in
            Expr.Sub(lhs, rhs)
        | Expr.Eq (lhs, rhs) ->
            let lhs = checkExpr (lhs) in
            let rhs = checkExpr (rhs) in

            if typ (lhs) <> typ (rhs) then
                failCheck
            else
                Expr.Eq(lhs, rhs) in

    if typ expr = Type.Unknown then failCheck else expr

and checkNode: Node -> Node =
    function
    | Expr (expr) -> Node.Expr(checkExpr (expr))

and check (p: Program) : Program = { Nodes = List.map checkNode p.Nodes }
