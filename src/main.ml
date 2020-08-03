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
       print_string (" (" ^ (Option.value arg ~default:"None") ^ ", ");
       Ast._print_ast body; print_string ")"

  type _scope = (string, sexp_object) hashtbl
  (* inner scope, (optional) outer scope *)
  and scope = Scope of _scope * scope option

  let rec print_scope (scope : scope) : unit =
    let Scope (s, s') = scope in
    match s' with
    | None ->
       print_string "(scope/global) ";
       Hashtbl.iter (fun x y -> print_string x; print_string " -> "; print_sexp_object y; print_string "; ") s;
       print_endline "";
    | Some s'' ->
       print_string "(scope/local) ";
       Hashtbl.iter (fun x y -> print_string x; print_string " -> "; print_sexp_object y; print_string "; ") s;
       print_endline "";
       print_scope s''

  let rec find_in_scope (a : string) (scope : scope) : (sexp_object, string) result =
    let try_once (s : _scope) : (sexp_object, string) result =
      print_string "(searching scope) ";
      Hashtbl.iter (fun x y -> print_string x; print_string " -> "; print_sexp_object y; print_string "; ") s;
      print_endline "";
      try
        Ok (Hashtbl.find s a)
      with
      | Not_found -> Error ("Binding " ^ a ^ " not found in the scope.")
    in
    match scope with
    | Scope (s, None) -> try_once s
    | Scope (s, Some s') ->
       match try_once s with
       | Ok s -> Ok s
       | Error _ -> find_in_scope a s'

  let add_to_scope (key : string) (value : sexp_object) (scope : scope) : unit =
    Hashtbl.add
      (match scope with
       | Scope (s, _) -> s
      )
      key value

  (* (define id (lambda (x) x)) should return id
   * id should return #<procedure id>
   * (lambda (x) x) should return #<procedure nil> *)

  let rec eval_in_scope (expr : Ast.sexp) (scope : scope) : (sexp_object, string) result =
    match expr with
    (* if it's a literal, we just return it *)
    | Ast.Atom a ->
       (match a with
        (* if it's a literal, try to find an associated value with it *)
        | Ast.Literal s -> find_in_scope s scope
        (* otherwise, just return it as-is *)
        | _ -> Ok (Object expr)
       )
    (* if it's a cons, then it must be a function call *)
    | Ast.Cons (head, tail) ->
       (match head with
        (* if it's an atom, we have to check what kind of atom is it *)
        | Ast.Atom b ->
           (match b with
            | Ast.Literal c ->
               (match c with
                | "define" -> eval_define_body_in_scope tail scope
                | "lambda" -> eval_lambda_body_in_scope tail scope
                | _ ->
                   (* first we try to find identifier in the current scope *)
                   (match find_in_scope c scope with
                    | Error e -> Error e
                    | Ok c' ->
                       (match c' with
                        (* if it's object, it's invalid *)
                        | Object _ -> Error "Non-procedure application."
                        | Procedure (arg, body) -> eval_procedure_application_in_scope (arg, body) tail scope
                       )
                   )
               )
            | _ -> Error "Invalid expression."
           )
        (* if a is cons, then we need to evaluate it first *)
        | Ast.Cons _ ->
           (match eval_in_scope head scope with
            | Error e -> Error e
            | Ok s ->
               (match s with
                | Object _ -> Error "Invalid language construction, something went wrong."
                | Procedure (arg, body) -> eval_procedure_application_in_scope (arg, body) tail scope
               )
           )
       )
  and eval_procedure_application_in_scope ((arg, body) : (string option * Ast.sexp)) (tail : Ast.sexp) (scope : scope) : (sexp_object, string) result =
    (match arg with
     | None -> eval_in_scope body scope
     | Some arg_name ->
        (match Ast.car tail with
         | None -> Error "Expected cons cell."
         | Some arg ->
            (match eval_in_scope arg scope with
             | Error e -> Error e
             | Ok arg' ->
                let local_scope = Hashtbl.create 1 in
                Hashtbl.add local_scope arg_name arg';
                print_string "(constructed scope) ";
                Hashtbl.iter (fun x y -> print_string x; print_string " -> "; print_sexp_object y; print_string " ") local_scope;
                print_endline "";
                print_scope scope;
                let scope' = Scope (local_scope, Some scope) in
                print_scope scope';
                eval_in_scope body scope'
            )
        )
    )
  and eval_define_body_in_scope (expr : Ast.sexp) (scope : scope) : (sexp_object, string) result =
    (match expr with
     | Ast.Atom _ -> Error "Invalid expression, expected cons."
     | Ast.Cons (d, d') ->
        (match d with
         | Ast.Cons _ -> Failure.not_implemented ()
         | Ast.Atom e ->
            (match e with
             | Ast.Literal e' ->
                (match Ast.car d' with
                 | None -> Error "Expected cons cell."
                 | Some d'' ->
                    (match eval_in_scope d'' scope with
                     | Error e -> Error e
                     | Ok s ->
                        add_to_scope e' s scope;
                        Ok (Object d)
                    )
                )
             | _ -> Error "Expected literal as a binding name."
            )
        )
    )
  and eval_lambda_body_in_scope (expr : Ast.sexp) (scope : scope) : (sexp_object, string) result =
    (match expr with
     | Ast.Atom _ -> Error "Invalid expression, expected cons or nil."
     | Ast.Cons (d, d') ->
        (match d with
         | Ast.Atom e ->
            (match e with
             | Ast.Nil ->
                (match Ast.car d' with
                 | None -> Error "Expected cons cell."
                 | Some d'' -> Ok (Procedure (None, d''))
                )
             | _ -> Error "Invalid expression, expected cons or nil."
            )
         | Ast.Cons (Ast.Atom (Ast.Literal e), Ast.Atom Ast.Nil) ->
            (match Ast.car d' with
             | None -> Error "Expected cons cell."
             | Some d'' -> Ok (Procedure (Some e, d''))
            )
         | Ast.Cons _ -> Error "Currently not supported."
        )
    )
end

let main =
  let repl (scope : Eval.scope) : unit =
    print_string "> ";
    try
      let input = read_line () in
      let tokens = Lexer.lexer input in
      (match Parser.parser tokens with
       | Error e -> print_string ("!> " ^ e)
       | Ok (ast, tkns) ->
          print_string "\x1b[2m";
          print_string "#> "; Ast._print_ast ast; print_endline ""; print_string "*> "; Token.print_token_list tkns;
          print_endline "\x1b[0m";
          (match Eval.eval_in_scope ast scope with
           | Error e -> print_string ("!> " ^ e)
           | Ok s -> Eval.print_sexp_object s
          )
      );
      print_endline "";
    with
    | End_of_file -> exit 0
  in
  let _global_scope : Eval._scope = Hashtbl.create 64 in
  let global_scope = Eval.Scope (_global_scope, None) in
  while true do
    repl global_scope
  done
