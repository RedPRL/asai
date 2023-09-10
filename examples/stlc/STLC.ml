open Asai
open Bwd

open Syntax

module Terminal = Asai_tty.Make(ErrorCode)
module Logger = Asai.Logger.Make(ErrorCode)
module Server = Asai_lsp.Make(ErrorCode)

module Elab =
struct
  type env = (string * tp) bwd
  module Reader = Algaeff.Reader.Make (struct type nonrec env = env end)

  let bind_var nm tp k =
    Reader.scope (fun env -> Snoc(env, (nm, tp))) k

  let lookup ?loc nm =
    let ctx = Reader.read () in
    match Bwd.find_opt (fun (nm', _) -> String.equal nm nm') ctx with
    | Some (_, tp) -> tp
    | None ->
      Logger.fatalf ?loc `UnboundVariable "Variable '%s' is not in scope" nm

  let expected_connective ?loc conn tp =
    Logger.fatalf ?loc `TypeError "Expected a %s, but got %a." conn pp_tp tp

  let rec equate ?loc expected actual =
    Logger.tracef ?loc "When equating terms" @@ fun () ->
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
      Logger.fatalf ?loc `TypeError "Expected type %a, but got %a." pp_tp expected pp_tp actual

  let rec chk (tm : tm) (tp : tp) : unit =
    Logger.tracef ?loc:tm.loc "When checking against %a" Syntax.pp_tp tp @@ fun () ->
    match tm.value, tp with
    | Lam (nm, body), Fun (a, b) ->
      bind_var nm a @@ fun () ->
      chk body b
    | Lam (_, _), _ ->
      expected_connective ?loc:tm.loc "function type" tp
    | Pair (l, r), Tuple (a, b) ->
      chk l a;
      chk r b;
    | Pair (_, _), _ ->
      expected_connective ?loc:tm.loc "pair type" tp
    | Lit _, Nat ->
      ()
    | Lit _, _ ->
      expected_connective ?loc:tm.loc "ℕ" tp
    | Suc n, Nat ->
      chk n Nat
    | Suc _, _ ->
      expected_connective ?loc:tm.loc "ℕ" tp
    | _ ->
      let actual_tp = syn tm in
      equate ?loc:tm.loc tp actual_tp

  and syn (tm : tm) : tp =
    Logger.tracef ?loc:tm.loc "When synthesizing" @@ fun () ->
    match tm.value with
    | Var nm ->
      lookup ?loc:tm.loc nm
    | Ap (fn, arg) ->
      begin
        match syn fn with
        | Fun (a, b) ->
          chk arg a;
          b
        | tp ->
          expected_connective ?loc:tm.loc "function type" tp
      end
    | Fst tm ->
      begin
        match syn tm with
        | Tuple (l, _) ->
          l
        | tp ->
          expected_connective ?loc:tm.loc "pair type" tp
      end
    | Snd tm ->
      begin
        match syn tm with
        | Tuple (_, r) ->
          r
        | tp ->
          expected_connective ?loc:tm.loc "pair type" tp
      end
    | NatRec (z, s, scrut) ->
      begin
        let mot = syn z in
        chk s (Fun (mot, mot));
        chk scrut Nat;
        mot
      end
    | _ ->
      Logger.fatalf ?loc:tm.loc `TypeError "Unable to infer type"
end

module Driver =
struct
  let load_file filepath =
    let lexbuf = Lexing.from_channel (open_in filepath) in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filepath };
    let (tm, tp) =
      try Grammar.defn Lex.token lexbuf with
      | Lex.SyntaxError tok ->
        Logger.fatalf ~loc:(Span.of_lex lexbuf) `LexingError {|Unrecognized token "%s"|} (String.escaped tok)
      | Grammar.Error ->
        Logger.fatalf ~loc:(Span.of_lex lexbuf) `LexingError "Failed to parse"
    in
    Elab.Reader.run ~env:Emp @@ fun () ->
    Elab.chk tm tp

  let load mode filepath =
    let display =
      match mode with
      | `Debug -> Terminal.display ~display_traces:true
      | `Normal ->  Terminal.display ~display_traces:false
      | `Interactive -> Terminal.interactive_trace
    in
    Logger.run ~emit:display ~fatal:display @@ fun () ->
    load_file filepath

  let server () =
    Server.start
      ~init:(fun ~root:_ -> ())
      ~load_file:(fun ~display:push file -> Logger.run ~emit:push ~fatal:push @@ fun () -> load_file file)

end

let () =
  match Sys.argv.(1) with
  | "--server" -> Driver.server ()
  | "--debug" -> Driver.load `Debug Sys.argv.(2)
  | "--interactive" -> Driver.load `Interactive Sys.argv.(2)
  | filepath -> Driver.load `Normal filepath
