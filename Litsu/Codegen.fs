﻿(***********************************************************************)
(*                                                                     *)
(* Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.        *)
(*                                                                     *)
(* This Source Code Form is subject to the terms of the Mozilla Public *)
(* License, v. 2.0. If a copy of the MPL was not distributed with this *)
(* file, You can obtain one at https://mozilla.org/MPL/2.0/.           *)
(*                                                                     *)
(***********************************************************************)

module Litsu.Codegen

open Litsu.SyntaxTree
open Litsu.Type

let unreachable () = failwith "Unreachable"

type Context = { IndentN: int }

let private newContext () : Context = { IndentN = 0 }

let private varname (name: string) : string = sprintf "LITSU_%s" name

let rec private genExpr (ctx: Context) (write: string -> unit) (expr: Expr) : unit =
    let rec f: Expr -> string =
        function
        | Expr.Int (n) -> sprintf "%d" n
        | Expr.Add (lhs, rhs) ->
            (match typInfix lhs rhs with
             | Type.Int -> sprintf "$(( %s + %s ))" (f lhs) (f rhs)
             | _ -> unreachable ())
        | Expr.Sub (lhs, rhs) ->
            (match typInfix lhs rhs with
             | Type.Int -> sprintf "$(( %s - %s ))" (f lhs) (f rhs)
             | _ -> unreachable ())
        | Expr.Eq (lhs, rhs) ->
            let op =
                (match typ lhs with
                 | Type.Int -> "-eq"
                 | Type.Bool -> "=e"
                 | _ -> unreachable ())

            let cond = sprintf "[ \"%s\" %s \"%s\" ]" (f lhs) op (f rhs) in
            sprintf "$(%s && printf 'true' || printf 'false')" cond
        | Expr.Let (name, typ, init, body) ->
            let mutable out = sprintf "$(%s=\"%s\"\n" (varname name) (f init)
            let writeInner s = out <- out + s
            genExpr { ctx with IndentN = ctx.IndentN + 1 } writeInner body
            out <- out + ")"
            out
        | Expr.Var (name, _typ) -> sprintf "${%s}" (varname name)

    write (String.replicate ctx.IndentN "  ")
    write (sprintf "printf '%%s\\n' \"%s\"\n" (f expr)) |> ignore

let private genNode (ctx: Context) (write: string -> unit) (node: Node) : unit =
    match node with
    | Expr (expr) -> genExpr ctx write expr

let codegen (write: string -> unit) (prog: Program) : unit =
    List.map (genNode (newContext ()) write) prog.Nodes |> ignore