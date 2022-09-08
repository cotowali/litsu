(***********************************************************************)
(*                                                                     *)
(* Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.        *)
(*                                                                     *)
(* This Source Code Form is subject to the terms of the Mozilla Public *)
(* License, v. 2.0. If a copy of the MPL was not distributed with this *)
(* file, You can obtain one at https://mozilla.org/MPL/2.0/.           *)
(*                                                                     *)
(***********************************************************************)

module Litsu.Typing

open FSharp.Compatibility.OCaml
open Litsu.Type
open Litsu.TypeEnv
open Litsu.SyntaxTree

exception UnifyException of Type * Type

let rec occur r1 =
    function
    | Type.Var (r2) when r1 == r2 -> true
    | Type.Var ({ contents = None }) -> false
    | Type.Var ({ contents = Some (t2) }) -> occur r1 t2
    | _ -> false

let rec unify (t1: Type) (t2: Type) =
    match t1, t2 with
    | Type.Int, Type.Int
    | Type.Bool, Type.Bool -> ()
    | Type.Var (r1), Type.Var (r2) when r1 == r2 -> ()
    | Type.Var ({ contents = Some (t1') }), _ -> unify t1' t2
    | _, Type.Var ({ contents = Some (t2') }) -> unify t1 t2'
    | Type.Var ({ contents = None } as r1), _ ->
        if occur r1 t2 then
            raise (UnifyException(t1, t2))

        r1 := Some(t2)
    | _, Type.Var ({ contents = None } as r2) ->
        if occur r2 t1 then
            raise (UnifyException(t1, t2))

        r2 := Some(t1)
    | _, _ -> raise (UnifyException(t1, t2))


let rec derefType: Type -> Type =
    function
    | Type.Var ({ contents = None } as r) ->
        r := Some(Type.Unknown)
        Type.Unknown
    | Type.Var ({ contents = Some (t) } as r) ->
        let t = derefType t in
        r := Some(t)
        t
    | t -> t

let rec derefExpr (expr: Expr) : Expr =
    match expr with
    | Expr.Int (n) -> Expr.Int(n)
    | Expr.Infix (op, lhs, rhs, t) -> Expr.Infix(op, derefExpr lhs, derefExpr rhs, derefType t)
    | Expr.Let (name, typ, e1, e2) -> Let(name, derefType typ, derefExpr e1, derefExpr e2)
    | Expr.Var (name, typ) -> Expr.Var(name, derefType typ)

and derefNode: Node -> Node =
    function
    | Expr (expr) -> Node.Expr(derefExpr expr)

let deref (p: Program) : Program = { Nodes = List.map derefNode p.Nodes }

let rec infer (env: TypeEnv) (e: Expr) : Type =
    match e with
    | Expr.Int (_) -> Type.Int
    | Expr.Infix (op, lhs, rhs, t) ->
        let t1 = (infer env lhs)
        let t2 = (infer env rhs)
        unify t1 t2

        match op with
        | "=" -> unify t Type.Bool
        | "+"
        | "-" -> unify t t1
        | _ -> failwith (sprintf "Unknown operator `%s`" op)

        t
    | Expr.Let (name, t, e1, e2) ->
        unify t (infer env e1)
        infer (TypeEnv.add name t env) e2
    | Expr.Var (name, t) ->
        if TypeEnv.exists name env then
            let t' = TypeEnv.find name env in
            unify t t'
            t'
        else
            failwith (sprintf "Undefined variable %s" name)

let check (p: Program) : Program =
    let env = TypeEnv []

    List.map
        (function
        | Expr (e) -> infer env e)
        p.Nodes
    |> ignore

    deref p
