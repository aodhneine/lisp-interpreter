module Token = struct
  type token = OpenBracket
             | CloseBracket
             | Point
             | String of string
             | Identifier of string
             | Unknown of string

  let rec print_token (token : token) : unit =
    match token with
    | OpenBracket -> print_string "(openbracket)"
    | CloseBracket -> print_string "(closebracket)"
    | Point -> print_string "(point)"
    | String s -> print_string ("(string " ^ s ^ ")")
    | Identifier s -> print_string ("(identifier " ^ s ^ ")")
    | Unknown s -> print_string ("(unknown " ^ s ^ ")")
end

module Lexer = struct
  let lexer (input : string) : Token.token list =
    let l = String.length input in
    let rec lex (tokens : Token.token list) (i : int) : Token.token list =
      if i == l then tokens
      else
      let c = String.get input i in
      print_endline ("\x1b[2m(lex)\x1b[0m c = " ^ String.make 1 c);
      match c with
      | ' ' -> lex tokens (i + 1)
      | '(' -> lex (OpenBracket :: tokens) (i + 1)
      | ')' -> lex (CloseBracket :: tokens) (i + 1)
      | '.' -> lex (Point :: tokens) (i + 1)
      | '"' -> let rec loop (j : int) : string =
                 let c' = String.get input j in
                 match c' with
                 | '"' -> String.sub input i (j - i + 1)
                 | _ -> loop (j + 1)
               in
               let s = loop (i + 1) in
               print_endline ("\x1b[2m(lex)\x1b[0m s = " ^ s);
               lex (String s :: tokens) (i + String.length s)
      | 'a'..'z' -> let rec loop (j : int) : string =
                      let c' = String.get input j in
                      match c' with
                      (* terminals *)
                      | '(' | ')' | ' ' -> String.sub input i (j - i)
                      | _ -> loop (j + 1)
                    in
                    let s = loop i in
                    print_endline ("\x1b[2m(lex)\x1b[0m s = " ^ s);
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
           | Nil (* () or nil *)
           | True (* #t *)
           | False (* #f *)
  and cons = sexp * sexp

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
    print_string "("; _print_ast a; print_string " "; _print_ast b; print_string ")"

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
end

let main =
  Ast._print_ast (Ast.Cons ((Ast.Atom (Ast.Literal "print")), (Ast.Atom (Ast.String "nyan~"))));
  print_endline "";
  Ast._print_ast (Ast.Cons ((Ast.Atom (Ast.True)), (Ast.Atom (Ast.Nil))));
  print_endline "";
  ()

let _ =
  let repl () : unit =
    print_string "> ";
    let input = read_line() in
    let tokens = Lexer.lexer input in
    print_string "[ ";
    List.iter (fun x -> Token.print_token x; print_string "; ") tokens;
    print_endline "]";
  in
  while true do
    repl ()
  done
