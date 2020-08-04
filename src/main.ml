module Token = struct
  type token = OpenBracket
             | CloseBracket
             | Point
             (* this is only used in a combination with other characters, for example
              * #t, #f, #v(1, 2, 3) *)
             | Hash of char
             | Quote
             | String of string
             | Identifier of string
             | Unknown of string

  let rec print_token (token : token) : unit =
    match token with
    | OpenBracket -> print_string "(openbracket)"
    | CloseBracket -> print_string "(closebracket)"
    | Point -> print_string "(point)"
    | Hash c -> print_string ("(hash " ^ (String.make 1 c) ^ ")")
    | Quote -> print_string "(quote)"
    | String s -> print_string ("(string " ^ s ^ ")")
    | Identifier s -> print_string ("(identifier " ^ s ^ ")")
    | Unknown s -> print_string ("(unknown " ^ s ^ ")")

  let print_token_list (tokens : token list) : unit =
    print_string "[ ";
    List.iter (fun x -> print_token x; print_string "; ") tokens;
    print_string "]"
end

module Lexer = struct
  let lexer (input : string) : Token.token list =
    let l = String.length input in
    let rec lex (tokens : Token.token list) (i : int) : Token.token list =
      if i == l then tokens
      else
        (* we can safely use String.get *)
        let c = String.get input i in
        (* print_endline ("\x1b[2m(lex)\x1b[0m c = " ^ String.make 1 c); *)
        match c with
        | ' ' -> lex tokens (i + 1)
        | '(' -> lex (OpenBracket :: tokens) (i + 1)
        | ')' -> lex (CloseBracket :: tokens) (i + 1)
        | '.' -> lex (Point :: tokens) (i + 1)
        | '\'' -> lex (Quote :: tokens) (i + 1)
        | '#' -> lex (Hash (String.get input (i + 1)) :: tokens) (i + 2)
        | '"' -> let rec loop (j : int) : string =
                   let c' = String.get input j in
                   match c' with
                   | '"' -> String.sub input (i + 1) (j - i - 1)
                   | _ -> loop (j + 1)
                 in
                 let s = loop (i + 1) in
                 (* print_endline ("\x1b[2m(lex)\x1b[0m s = " ^ s); *)
                 lex (String s :: tokens) (i + String.length s + 2)
        | 'a'..'z' -> let rec loop (j : int) : string =
                        if j == l then String.sub input i (j - i)
                        else
                          let c' = String.get input j in
                          match c' with
                          (* terminals *)
                          | '(' | ')' | ' ' | '\'' -> String.sub input i (j - i)
                          | _ -> loop (j + 1)
                      in
                      let s = loop i in
                      (* print_endline ("\x1b[2m(lex)\x1b[0m s = " ^ s); *)
                      lex (Identifier s :: tokens) (i + String.length s)
        | c -> lex (Unknown (String.make 1 c) :: tokens) (i + 1)
    in
    List.rev (lex [] 0)
end

module Ast = struct
  type sexp = Atom of atom
            | Cons of cons
  and atom = String of string
           | Literal of string
           | Nil (* () *)
           | True (* #t *)
           | False (* #f *)
  and cons = sexp * sexp

  let car (s : sexp) : sexp option =
    match s with
    | Atom _ -> None
    | Cons (a, _) -> Some a

  let cdr (s : sexp) : sexp option =
    match s with
    | Atom _ -> None
    | Cons (_, b) -> Some b

  let print_atom (atom : atom) : unit =
    match atom with
    | String s -> print_string ("\"" ^ s ^ "\"")
    | Literal s -> print_string s
    | Nil -> print_string "nil"
    | True -> print_string "#t"
    | False -> print_string "#f"

  let rec _print_ast (ast : sexp) : unit =
    match ast with
    | Atom a -> print_atom a
    | Cons (a, b) -> _print_cons (a, b)
  and _print_cons ((a, b) : cons) : unit =
    print_string "("; _print_ast a; print_string " . "; _print_ast b; print_string ")"

  (* TODO: doesn't work properly for (<sexp> <sexp>...) *)
  let rec print_ast (ast : sexp) : unit =
    match ast with
    | Atom atom -> print_atom atom
    | Cons (a, b) -> print_cons (a, b)
  and print_cons ((a, b) : cons) : unit =
    match b with
    | Atom atom ->
       (match atom with
        | Nil -> print_string "("; print_ast a; print_string ")"
        | _ -> print_string "("; print_ast a; print_string " . "; print_atom atom; print_string ")"
       )
    | Cons (b', c) -> print_string "("; print_ast a; print_string " "; print_ast b; print_string ")"
end

module Parser = struct
  (* grammar:
   * sexp -> atom | cons
   * atom -> literal | string | nil | true | false
   * nil -> open_bracket close_bracket => Ast.Nil
   * true -> #t => Ast.True
   * false -> #f => Ast.False
   * cons -> open_bracket cons_list close_bracket
   * cons_list -> sexp => Ast.Cons a Ast.Nil
   *  | sexp point sexp => Ast.Cons a b
   *  | sexp cons_list => Ast.Cons a b
   *)

  let rec parser (input : Token.token list) : ((Ast.sexp * Token.token list), string) result =
    (* print_string "parsing "; Token.print_token_list input; print_endline ""; *)
    match List.hd input with
    | String s -> Ok ((Ast.Atom (Ast.String s)), List.tl input)
    | Identifier s -> Ok ((Ast.Atom (Ast.Literal s)), List.tl input)
    | Hash c ->
       (match c with
        | 't' -> Ok ((Ast.Atom Ast.True), List.tl input)
        | 'f' -> Ok ((Ast.Atom Ast.False), List.tl input)
        | _ -> Error "Unrecognised character after #."
       )
    | Quote ->
       (match parser (List.tl input) with
        | Error e -> Error e
        | Ok (a', b) -> Ok ((Ast.Cons (Ast.Atom (Ast.Literal "quote"), Ast.Cons (a', Ast.Atom Ast.Nil))), b)
       )
    | OpenBracket ->
       let a = List.tl input in
       (match List.hd a with
        | CloseBracket -> Ok ((Ast.Atom Ast.Nil), List.tl a)
        | _ -> parse_cons_list a
       )
    | CloseBracket -> Error "Unexpected closing bracket."
    | _ -> Error "Invalid token in the wrong place."
  and parse_cons_list (input : Token.token list) : ((Ast.sexp * Token.token list), string) result =
    match parser input with
    | Error e -> Error e
    | Ok (a, b) ->
       (match List.hd b with
        | CloseBracket -> Ok ((Ast.Cons (a, Ast.Atom Ast.Nil)), List.tl b)
        | Point ->
           let c = List.tl b in
           (match parser c with
            | Error e -> Error e
            | Ok (c', d) ->
               (match List.hd d with
                | CloseBracket -> Ok ((Ast.Cons (a, c')), List.tl d)
                | _ -> Error "Invalid token."
               )
           )
        | _ ->
           (match parse_cons_list b with
            | Error e -> Error e
            | Ok (b', c) -> Ok ((Ast.Cons (a, b')), c)
           )
       )
end

(* Why is this not in Stdlib? *)
type ('a, 'b) hashtbl = ('a, 'b) Hashtbl.t

module Failure = struct
  let not_implemented () : ('a, string) result = Error "Not implemented yet."
  let work_in_progress () : ('a, string) result = Error "Work In Progress."
end

module Eval = struct
  (* (lambda () "some string") should return procedure
   * ((lambda () "ayaya")) should evaluate to "ayaya"
   * ((lambda (name) name) "aodhneine") should evaluate to "aodhneine"
   *)

  type sexp_object =
    | Object of Ast.sexp
    | Procedure of string option * Ast.sexp

  let print_sexp_object (s : sexp_object) : unit =
    match s with
    | Object s -> Ast._print_ast s
    | Procedure (arg, body) ->
       print_string ("#<procedure nil>");

  module Scope = struct
    type _scope = (string, sexp_object) hashtbl
    (* inner scope, (optional) outer scope *)
    and t = Scope of _scope * t option

    let rec print (scope : t) : unit =
      match scope with
      | Scope (s, None) ->
         Hashtbl.iter (fun x y -> print_string x; print_string " : "; print_sexp_object y; print_string "; ") s;
         print_endline "";
      | Scope (s, Some s') ->
         Hashtbl.iter (fun x y -> print_string x; print_string " : "; print_sexp_object y; print_string "; ") s;
         print s'

    let rec find (key : string) (scope : t) : (sexp_object, string) result =
      let try_once (s : _scope) : (sexp_object, string) result =
        try
          Ok (Hashtbl.find s key)
        with
        | Not_found -> Error ("Binding " ^ key ^ " not found in the scope.")
      in
      match scope with
      | Scope (s, None) -> try_once s
      | Scope (s, Some s') ->
         match try_once s with
         | Ok s -> Ok s
         | Error _ -> find key s'

    let add (key : string) (value : sexp_object) (scope : t) : unit =
      Hashtbl.add
        (match scope with
         | Scope (s, _) -> s
        )
        key value
  end

  type scope = Scope.t

  (* (define id (lambda (x) x)) should return id
   * id should return #<procedure id>
   * (lambda (x) x) should return #<procedure nil> *)

  let rec eval_in_scope (expr : Ast.sexp) (scope : scope) : ((sexp_object * scope), string) result =
    match expr with
    | Ast.Atom atom -> eval_atom_in_scope atom scope
    | Ast.Cons (head, tail) ->
       match head with
       | Ast.Atom (Ast.Literal s) ->
          (match s with
           | "define" -> eval_define_body_in_scope tail scope
           | "quote" ->
              (match Ast.car tail with
               | Some body -> Ok ((Object body), scope)
               | None -> Error "Invalid language construction, expected cons."
              )
           | "lambda" -> eval_lambda_body_in_scope tail scope
           | _ ->
              (match Scope.find s scope with
               | Ok obj ->
                  (match obj with
                   | Procedure (args, body) -> eval_procedure_application_in_scope (args, body) tail scope
                   | Object _ -> Error "Non-procedure application."
                  )
               | Error e -> Error e
              )
          )
       | Ast.Cons _ ->
          (match eval_in_scope head scope with
           | Ok (obj, scope) ->
              (match obj with
               | Procedure (args, body) ->
                  eval_procedure_application_in_scope (args, body) tail scope
               | Object _ -> Error "Non-procedure application."
              )
           | Error e -> Error e
          )
       | Ast.Atom _ -> Error "Invalid language construction, expected literal or cons."
  and eval_atom_in_scope (atom : Ast.atom) (scope : scope) : ((sexp_object * scope), string) result =
    match atom with
    | Ast.Literal s ->
       (match Scope.find s scope with
        | Ok obj -> Ok (obj, scope)
        | Error e -> Error e
       )
    | _ -> Ok ((Object (Ast.Atom atom)), scope)
  and eval_define_body_in_scope (body : Ast.sexp) (scope : scope) : ((sexp_object * scope), string) result =
    match body with
    | Ast.Cons (name, body) ->
       (match name with
        | Ast.Atom (Ast.Literal s) ->
           (match Ast.car body with
            | Some body' ->
               (match eval_in_scope body' scope with
                | Ok (evaluated_form, _) ->
                   Scope.add s evaluated_form scope;
                   Ok ((Object name), scope)
                | Error e -> Error e
               )
            | None -> Error "Invalid language construction, expected cons."
           )
        | _ -> Error "Invalid language construction, expected literal."
       )
    | Ast.Atom _ -> Error "Invalid language construction, expected cons."
  and eval_lambda_body_in_scope (body : Ast.sexp) (scope : scope) : ((sexp_object * scope), string) result =
    match body with
    | Ast.Cons (args, body) ->
       (match
          (match args with
           | Ast.Atom Ast.Nil -> Ok None
           | Ast.Cons (a, rest) ->
              (match rest with
               | Ast.Atom Ast.Nil ->
                  (match a with
                   | Ast.Atom (Ast.Literal s) -> Ok (Some s)
                   | _ -> Error "Invalid language construction, expected literal."
                  )
               | _ -> Failure.not_implemented ()
              )
           | _ -> Error "Invalid language construction, expected cons or nil."
          )
        with
        | Error e -> Error e
        | Ok args ->
           (match Ast.car body with
            | Some body -> Ok ((Procedure (args, body)), scope)
            | None -> Error "Invalid language construction, expected cons."
           )
       )
    | Ast.Atom _ -> Error "Invalid language construction, expected cons."
  and eval_procedure_application_in_scope ((args, body) : string option * Ast.sexp) (tail : Ast.sexp) (scope : scope) : ((sexp_object * scope), string) result =
    match args with
    | None -> eval_in_scope body scope
    | Some arg_name ->
       match Ast.car tail with
       | Some arg ->
          (match eval_in_scope arg scope with
           | Ok (arg', scope) ->
              let _inner_scope : Scope._scope = Hashtbl.create 1 in
              Hashtbl.add _inner_scope arg_name arg';
              eval_in_scope body (Scope.Scope (_inner_scope, Some scope))
           | Error e -> Error e
          )
       | None -> Error "Invalid language construction, expected cons."
end

let main =
  let repl (scope : Eval.scope) : unit =
    print_string "> ";
    try
      let input = read_line () in
      (* TODO: This has no bound checking or anything. *)
      if String.get input 0 == ',' then
        match String.sub input 1 (String.length input - 1) with
        | "quit" -> exit 0
        | "scope" -> Eval.Scope.print scope
        | _ -> ()
      else
        let tokens = Lexer.lexer input in
        (match Parser.parser tokens with
         | Error e -> print_string ("!> " ^ e)
         | Ok (ast, tkns) ->
            print_string "\x1b[2m";
            print_string "#> "; Ast._print_ast ast; print_endline ""; print_string "*> "; Token.print_token_list tkns;
            print_endline "\x1b[0m";
            (match Eval.eval_in_scope ast scope with
             | Error e -> print_string ("!> " ^ e)
             | Ok (s, _) -> Eval.print_sexp_object s
            )
        );
        print_endline "";
    with
    | End_of_file -> exit 0
  in
  let global_scope = Eval.Scope.Scope (Hashtbl.create 64, None) in
  while true do
    repl global_scope
  done
