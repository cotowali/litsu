(***********************************************************************)
(*                                                                     *)
(* Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.        *)
(*                                                                     *)
(* This Source Code Form is subject to the terms of the Mozilla Public *)
(* License, v. 2.0. If a copy of the MPL was not distributed with this *)
(* file, You can obtain one at https://mozilla.org/MPL/2.0/.           *)
(*                                                                     *)
(***********************************************************************)

module Litsu.Codegen

open FSharp.Compatibility.OCaml.Pervasives
open Litsu.SyntaxTree
open Litsu.Type

let unreachable () = failwith "Unreachable"

type Context = { IndentN: int }

let private newContext () : Context = { IndentN = 0 }

let private newInnerContext (ctx: Context) : Context = { ctx with IndentN = ctx.IndentN + 1 }

let private varname (name: string) : string = sprintf "LITSU_%s" name

let private sTrue, sFalse = ("true", "false")

let private indentStr (ctx: Context) : string = String.replicate ctx.IndentN "  "

let rec private genExpr (ctx: Context) (write: string -> unit) (expr: Expr) : unit =
    let rec f: Expr -> string =
        function
        | Expr.Int (n) -> sprintf "%d" n
        | Expr.String (s) ->
            String.collect
                (function
                | '"' -> "\\\""
                | '\\' -> "\\\\"
                | '$' -> "\\$"
                | c -> sprintf "%c" c)
                s
        | Expr.Infix (op, lhs, rhs, t) ->
            (match op with
             | "+"
             | "-" ->
                 (match t with
                  | Type.Int -> sprintf "$(( ( %s ) %s ( %s ) ))" (f lhs) op (f rhs)
                  | _ -> unreachable ())
             | "=" ->
                 let op =
                     (match t with
                      | Type.Int -> "-eq"
                      | Type.Bool -> "="
                      | _ -> unreachable ())

                 let cond = sprintf "[ \"%s\" %s \"%s\" ]" (f lhs) op (f rhs) in
                 sprintf "$(%s && printf '%s' || printf '%s')" cond sTrue sFalse
             | _ -> unreachable ())
        | Expr.If (cond, e1, e2, _) ->
            let mutable out = sprintf "$(if [ \"%s\" = '%s' ]\n" (f cond) sTrue
            let writeInner s = out <- out + s
            writeInner "then\n"
            genExpr { ctx with IndentN = ctx.IndentN + 1 } writeInner e1
            writeInner "else\n"
            genExpr { ctx with IndentN = ctx.IndentN + 1 } writeInner e2
            writeInner "fi)"
            out
        | Expr.Let (name, typ, args, init, body) ->
            let mutable out = "$("
            let writeInner s = out <- out + s
            let ctx = newInnerContext ctx

            if List.length args > 0 then
                let fname = varname name
                writeInner (sprintf "%s() {\n" fname)

                (fun () ->
                    let ctx = newInnerContext ctx in

                    List.iteri
                        (fun i name ->
                            writeInner (sprintf "%s%s=$%d\n" (indentStr ctx) (varname name) (i + 1)))
                        (List.map fst args)

                    genExpr ctx writeInner init)
                    ()

                writeInner (sprintf "%s}\n" (indentStr ctx))
                // This allows calling function with "${name} args"
                writeInner (sprintf "%s%s=%s\n" (indentStr ctx) fname fname)
            else
                writeInner (sprintf "%s=\"%s\"\n" (varname name) (f init))

            genExpr ctx writeInner body

            writeInner ")"
            out
        | Expr.App (fe, args, _) -> sprintf "$(%s %s)" (f fe) (String.concat " " (List.map f args))
        | Expr.Var (name, _typ) -> sprintf "${%s}" (varname name)

    write (indentStr ctx)
    write (sprintf "printf '%%s\\n' \"%s\"\n" (f expr)) |> ignore

let private genNode (ctx: Context) (write: string -> unit) (node: Node) : unit =
    match node with
    | Expr (expr) -> genExpr ctx write expr

let codegen (write: string -> unit) (prog: Program) : unit =
    List.map (genNode (newContext ()) write) prog.Nodes |> ignore
