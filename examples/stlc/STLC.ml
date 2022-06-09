open Bwd

open Kai.Loc

open Syntax

module Terminal = Kai_unix.Make(Doctor.ErrorCode)

module Elab =
struct
  module Reader = Algaeff.Reader.Make (struct type env = (string * tp) bwd end)

  let bind_var nm tp k =
    Reader.scope (fun env -> Snoc(env, (nm, tp))) k

  let lookup nm =
    let env = Reader.read () in
    match Bwd.find_opt (fun (nm', _) -> String.equal nm nm') env with
    | Some (_, tp) -> tp
    | None ->
      let msg = Format.asprintf "Variable '%s' is not in scope." nm in
      Doctor.build ~code:UnboundVariable msg
      |> Doctor.cause "This variable was not in scope."
      |> Doctor.fatal

  let expected_connective conn tp =
    let msg = Format.asprintf "Expected a %s, but got %a." conn pp_tp tp in
    let cause = Format.asprintf "I expected this to be %s." conn in
    Doctor.build ~code:TypeError msg
    |> Doctor.cause cause
    |> Doctor.fatal

  let rec equate expected actual =
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
      let msg = Format.asprintf "Expected type %a, but got %a." pp_tp expected pp_tp actual in
      Doctor.build ~code:TypeError msg
      |> Doctor.cause "This had the wrong type!"
      |> Doctor.fatal

  let rec chk (tm : tm) (tp : tp) : unit =
    Doctor.locate tm.span @@ fun () ->
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
    Doctor.locate tm.span @@ fun () ->
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
      let msg = "Unable to infer type." in
      Doctor.build ~code:TypeError msg
      |> Doctor.cause "I couldn't infer the type of this term."
      |> Doctor.fatal
end

module Driver =
struct
  let load filepath =
    Doctor.run ~display:Terminal.display @@ fun () ->
    let lexbuf = Lexing.from_channel (open_in filepath) in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filepath };
    let contents = 
      let ch = open_in filepath in
      let str = really_input_string ch (in_channel_length ch) in
      close_in ch;
      str
    in
    Doctor.load_file ~filepath @@ contents;
    let (tm, tp) =
      try Grammar.defn Lex.token lexbuf with
      | Lex.SyntaxError tok ->
        Doctor.position (Pos.create @@ lexbuf.lex_curr_p) @@ fun () ->
        let msg = Format.asprintf "Unrecognized token '%s'." tok in
        Doctor.build ~code:LexerError msg
        |> Doctor.cause "I could not recognize this token."
        |> Doctor.fatal
      | Grammar.Error ->
        Doctor.position (Pos.create @@ lexbuf.lex_curr_p) @@ fun () ->
        let msg = "Failed to parse." in
        Doctor.build ~code:LexerError msg
        |> Doctor.cause "I couldn't figure out how to parse this."
        |> Doctor.fatal
    in
    Elab.Reader.run ~env:Emp @@ fun () ->
    Elab.chk tm tp
end

let () =
  exit @@ Driver.load "./examples/stlc/example.lambda"
