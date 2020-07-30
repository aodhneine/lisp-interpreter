module Token = struct
  type token = OpenBracket
             | CloseBracket
             | Point
             | String of string
             | Identifier of string
             | Unknown of string
  
  let rec print_token (token : token) : unit =
    match token with
    | OpenBracket -> print_string "OpenBracket"
    | CloseBracket -> print_string "CloseBracket"
    | Point -> print_string "Point"
    | String s -> print_string ("String(" ^ s ^ ")")
    | Identifier s -> print_string ("Identifier(" ^ s ^ ")")
    | Unknown s -> print_string ("Unknown(" ^ s ^ ")")
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
  and atom = string
  and cons = sexp * sexp
end

module Parser = struct
end

let main =
  let repl () : unit =
    print_string "> ";
    let input = read_line() in
    let tokens = Lexer.lexer input in
    print_string "[ ";
    List.iter (fun x -> Token.print_token x; print_string "; ") tokens;
    print_endline "]"
  in
  while true do
    repl ()
  done
