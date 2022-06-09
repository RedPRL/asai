{
open Lexing
open Grammar

exception SyntaxError of string

let make_table num elems =
  let table = Hashtbl.create num in
  List.iter (fun (k, v) -> Hashtbl.add table k v) elems;
  table

let keywords =
  make_table 0 [
    ("check", CHECK);
    ("ℕ", NAT);
    ("pair", PAIR);
    ("fst", FST);
    ("snd", SND);
    ("suc", SUC);
    ("nat-rec", NAT_REC);
  ]
}

let line_ending
  = '\r'
  | '\n'
  | "\r\n"
let whitespace =
  [' ' '\t']+

let atom_initial =
  [^ '0'-'9' '-'     '?' '!' '(' ')' '[' ']' '{' '}' '<' '>' '.' '#' '\\' '@' '*' '^' ':' ',' ';' '|' '=' '"' '`' ' ' '\t' '\n' '\r']

let atom_subsequent =
  [^                         '(' ')' '[' ']' '{' '}' '<' '>' '.' '#' '\\' '@' '*' '^' ':' ',' ';' '|' '=' '"'     ' ' '\t' '\n' '\r']

let atom = atom_initial atom_subsequent*

rule skip_whitespace kont = parse
  | line_ending
    { new_line lexbuf; (skip_whitespace kont) lexbuf }
  | whitespace
    { skip_whitespace kont lexbuf }
  | ""
    { kont lexbuf }

and token = parse "" { skip_whitespace real_token lexbuf }

and real_token = parse
  | "λ"
    { LAMBDA }
  | "→"
    { ARROW }
  | "×"
    { TIMES }
  | atom
    {
      (* [TODO: Reed M, 02/05/2022] Actually disallow subscripts *)
      (* See [NOTE: Pretty Printing + Renaming Variables] to see why we disallow numeric unicode subscripts. *)
      let input = lexeme lexbuf in
      match Hashtbl.find keywords input with
      | tok -> tok
      | exception Not_found -> Grammar.ATOM input
    }
  | eof
    { EOF }
  | _
    { raise @@ SyntaxError (lexeme lexbuf) }