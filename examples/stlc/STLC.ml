open Asai
open Bwd

open Syntax

module Terminal = Asai_unix.Make(Doctor.Code)

module Elab =
struct
  type env = {ctx : (string * tp) bwd ; loc : Span.t option}
  module Reader = Algaeff.Reader.Make (struct type nonrec env = env end)

  let bind_var nm tp k =
    Reader.scope (fun env -> {env with ctx = Snoc(env.ctx, (nm, tp))}) k

  let locate loc k =
    Reader.scope (fun env -> {env with loc}) k

  let get_loc () =
    let env = Reader.read () in 
    env.loc

  let lookup nm =
    let {ctx ; loc} = Reader.read () in
    match Bwd.find_opt (fun (nm', _) -> String.equal nm nm') ctx with
    | Some (_, tp) -> tp
    | None ->
      Doctor.fatalf ?loc ~code:UnboundVariable "Variable '%s' is not in scope" nm

  let expected_connective conn tp =
    Doctor.fatalf ?loc:(get_loc ()) ~code:TypeError  "Expected a %s, but got %a." conn pp_tp tp

  let rec equate expected actual =
    Doctor.tracef "When Equating" @@ fun () ->
    match expected, actual with
    | Fun (a0, b0), Fun (a1, b1) ->
      equate a0 a1;
      equate b0 b1
    | Tuple (a0, b0), Tuple (a1, b1) ->
      equate a0 a1;
      equate b0 b1
    | Nat, Nat ->
      ()
    | _, _ ->
      Doctor.fatalf ?loc:(get_loc ()) ~code:TypeError "Expected type %a, but got %a." pp_tp expected pp_tp actual

  let rec chk (tm : tm) (tp : tp) : unit =
    Doctor.tracef ?loc:tm.loc "When checking against %a" Syntax.pp_tp tp @@ fun () ->
    locate tm.loc @@ fun () ->
    match tm.value, tp with
    | Lam (nm, body), Fun (a, b) ->
      bind_var nm a @@ fun () ->
      chk body b
    | Lam (_, _), _ ->
      expected_connective "function type" tp
    | Pair (l, r), Tuple (a, b) ->
      chk l a;
      chk r b;
    | Pair (_, _), _ ->
      expected_connective "pair type" tp
    | Lit _, Nat ->
      ()
    | Lit _, _ ->
      expected_connective "ℕ" tp
    | Suc n, Nat ->
      chk n Nat
    | Suc _, _ ->
      expected_connective "ℕ" tp
    | _ ->
      let actual_tp = syn tm in
      equate tp actual_tp

  and syn (tm : tm) : tp =
    Doctor.tracef ?loc:tm.loc "When synthesizing" @@ fun () ->
    locate tm.loc @@ fun () ->
    match tm.value with
    | Var nm ->
      lookup nm
    | Ap (fn, arg) ->
      begin
        match syn fn with
        | Fun (a, b) ->
          chk arg a;
          b
        | tp ->
          expected_connective "function type" tp
      end
    | Fst tm ->
      begin
        match syn tm with
        | Tuple (l, _) ->
          l
        | tp ->
          expected_connective "pair type" tp
      end
    | Snd tm ->
      begin
        match syn tm with
        | Tuple (_, r) ->
          r
        | tp ->
          expected_connective "pair type" tp
      end
    | NatRec (z, s, scrut) ->
      begin
        let mot = syn z in
        chk s (Fun (mot, mot));
        chk scrut Nat;
        mot
      end
    | _ ->
      Doctor.fatalf ?loc:(get_loc ()) ~code:TypeError "Unable to infer type"
end

module Driver =
struct
  let load_file filepath =
    let lexbuf = Lexing.from_channel (open_in filepath) in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filepath };
    (* let contents = 
       let ch = open_in filepath in
       let str = really_input_string ch (in_channel_length ch) in
       close_in ch;
       str
       in
       Doctor.load_file ~filepath @@ contents; *)
    let (tm, tp) =
      try Grammar.defn Lex.token lexbuf with
      | Lex.SyntaxError tok ->
        let pos = Span.of_lex_position lexbuf.lex_curr_p in
        Doctor.fatalf ~loc:(Span.make pos pos) ~code:LexerError "Unrecognized token '%s'" tok
      | Grammar.Error ->
        let pos = Span.of_lex_position lexbuf.lex_curr_p in
        Doctor.fatalf ~loc:(Span.make pos pos) ~code:LexerError "Failed to parse"
    in
    Elab.Reader.run ~env:{ctx = Emp ; loc = None} @@ fun () ->
    Elab.chk tm tp

  let load mode filepath =
    let display = 
      match mode with
      | `Debug -> Terminal.display ~display_traces:true
      | `Normal ->  Terminal.display ~display_traces:false
      | `Interactive -> Terminal.interactive_trace
    in
    Doctor.run ~emit:display ~fatal:display @@ fun () ->
    load_file filepath

end

let () =
  let mode = 
    if Array.length Sys.argv < 2 then
      `Normal
    else
      match Sys.argv.(2) with
      | "--debug" | "-d" -> `Debug
      | "--interactive" | "-i" -> `Interactive
      | _ -> failwith "Unrecognized argument"
  in
  Driver.load mode Sys.argv.(1)