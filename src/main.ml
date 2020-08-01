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
        | '#' -> lex (Hash (String.get input (i + 1)) :: tokens) (i + 2)
        | '"' -> let rec loop (j : int) : string =
                   let c' = String.get input j in
                   match c' with
                   | '"' -> String.sub input i (j - i + 1)
                   | _ -> loop (j + 1)
                 in
                 let s = loop (i + 1) in
                 (* print_endline ("\x1b[2m(lex)\x1b[0m s = " ^ s); *)
                 lex (String s :: tokens) (i + String.length s)
        | 'a'..'z' -> let rec loop (j : int) : string =
                        if j == l then String.sub input i (j - i)
                        else
                          let c' = String.get input j in
                          match c' with
                          (* terminals *)
                          | '(' | ')' | ' ' -> String.sub input i (j - i)
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

  let print_atom (atom : atom) : unit =
    match atom with
    | String s -> print_string ("" ^ s)
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

  let rec print_ast (ast : sexp) : unit =
    match ast with
    | Atom atom -> print_atom atom
    | Cons (a, b) -> print_cons (a, b)
  and print_cons ((a, b) : cons) : unit =
    match b with
    | Atom b' -> (match b' with
                  | Nil -> print_string "("; print_ast a; print_string ")"
                  | _ -> print_string "("; print_ast a; print_string " . "; print_atom b'; print_string ")"
                 )
    | _ -> print_string "("; print_ast a; print_string " "; print_ast b; print_string ")"
end

module Parser = struct
  (* grammar:
   * sexp -> atom | cons
   * atom -> literal | string | nil | true | false
   * nil -> open_bracket close_bracket => Ast.Nil
   * true -> #t => Ast.True
   * false -> #f => Ast.False
   * cons -> open_bracket sexp point sexp close_bracket => Ast.Cons a b
   *  | open_bracket cons_list close_bracket
   * cons_list -> sexp => Ast.Cons a Ast.Nil
   *  | sexp cons_list => Ast.Cons a b
   *)

  let rec parser (input : Token.token list) : ((Ast.sexp * Token.token list), string) result =
    print_string "parsing "; Token.print_token_list input; print_endline "";
    match List.hd input with
    | String s -> Ok ((Ast.Atom (Ast.String s)), List.tl input)
    | Identifier s -> Ok ((Ast.Atom (Ast.Literal s)), List.tl input)
    | Hash c ->
       (match c with
        | 't' -> Ok ((Ast.Atom Ast.True), List.tl input)
        | 'f' -> Ok ((Ast.Atom Ast.False), List.tl input)
        | _ -> Error "Unrecognised character after #."
       )
    | OpenBracket ->
       let a = List.tl input in
       (match List.hd a with
        | CloseBracket -> Ok ((Ast.Atom Ast.Nil), List.tl a)
        | _ ->
           (match parser a with
            | Error e -> Error e
            | Ok (a', b) ->
               (* Now we have first element - a', and rest of the tokens - b *)
               (match List.hd b with
                | CloseBracket -> Ok ((Ast.Cons (a', Ast.Atom Ast.Nil)), List.tl b)
                | Point ->
                   let c = List.tl b in
                   (match parser c with
                    | Error e -> Error e
                    | Ok (c', d) ->
                       (* Now we have second element - c', and rest of the tokens - d *)
                       (match List.hd d with
                        | CloseBracket -> Ok ((Ast.Cons (a', c')), List.tl d)
                        | _ -> Error "Invalid token."
                       )
                   )
                | _ ->
                   Error "Not implemented yet."
               )
           )
       )
    | CloseBracket -> Error "Unexpected closing bracket."
    | _ -> Error "Invalid token in the wrong place."
end

module Evaluator = struct
(* language:
 * define -> (define <id> <sexp>) | (define (<sexp>) <sexp>)
 * lambda -> (lambda <sexp> <sexp>)
 * quote -> (lambda <sexp>)
 *)
end

module Result = struct
  include Result

  let penetrate (g : ('b -> 'd)) (f : ('a -> 'c)) (r: ('a, 'b) result) : ('c, 'd) result =
    Result.map_error g (Result.map f r)

  let drop (r: ('a, 'b) result) : unit =
    ()
end

let main =
  let repl () : unit =
    print_string "> ";
    let input = read_line() in
    let tokens = Lexer.lexer input in
    (* Token.print_token_list tokens; *)
    Result.drop
      (Result.penetrate
         (fun x -> print_string ("!> " ^ x))
         (fun (x, y) -> print_string "=> "; Ast._print_ast x; print_endline ""; print_string "*> "; Token.print_token_list y)
         (Parser.parser tokens));
    print_endline ""
  in
  while true do
    repl ()
  done
