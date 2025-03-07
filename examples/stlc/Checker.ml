open Bwd
open Syntax

module Terminal = Asai.Tty.Make(Reporter.Message)
module Github = Asai.Github.Make(Reporter.Message)
module Server = Asai_lsp.Make(Reporter.Message)

module Elab =
struct
  type env = (string * tp) bwd
  module Reader = Algaeff.Reader.Make (struct type t = env end)

  let bind_var nm tp k =
    Reader.scope (fun env -> Snoc(env, (nm, tp))) k

  let lookup nm =
    let ctx = Reader.read () in
    match Bwd.find_opt (fun (nm', _) -> String.equal nm nm') ctx with
    | Some (_, tp) -> tp
    | None ->
      Reporter.fatalf Unbound_variable "variable '%s' is not in scope" nm

  let expected_connective conn tp =
    Reporter.fatalf Type_error "expected a %s, but got %a" conn pp_tp tp

  let rec equate expected actual =
    Reporter.trace "when equating terms" @@ fun () ->
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
      Reporter.fatalf Type_error "expected type %a, but got %a" pp_tp expected pp_tp actual

  let rec chk (tm : tm) (tp : tp) : unit =
    Reporter.tracef ?loc:tm.loc "when checking it against %a" Syntax.pp_tp tp @@ fun () ->
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
    Reporter.trace ?loc:tm.loc "when synthesizing its type" @@ fun () ->
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
    | Nat_rec (z, s, scrut) ->
      begin
        let mot = syn z in
        chk s (Fun (mot, mot));
        chk scrut Nat;
        mot
      end
    | _ ->
      Reporter.fatal Type_error "unable to infer its type"
end

module Driver =
struct
  let load_file filepath =
    let lexbuf = Lexing.from_channel (open_in filepath) in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filepath };
    let (tm, tp) =
      try Grammar.defn Lex.token lexbuf with
      | Lex.SyntaxError tok ->
        Reporter.fatalf ~loc:(Asai.Range.of_lexbuf lexbuf) Parsing_error "unrecognized token %S" tok
      | Grammar.Error ->
        Reporter.fatal ~loc:(Asai.Range.of_lexbuf lexbuf) Parsing_error "failed to parse"
    in Elab.Reader.run ~env:Emp @@ fun () -> Elab.chk tm tp

  let load mode filepath =
    let display : Reporter.Message.t Asai.Diagnostic.t -> unit =
      match mode with
      | `Normal -> fun d -> Terminal.display d
      | `Github -> fun d -> Github.print d
    in
    Reporter.run ~emit:display ~fatal:(fun d -> display d; exit 1) @@ fun () ->
    load_file filepath

  let server () =
    Server.start
      ~source:(Some "STLC")
      ~init:(fun ~root:_ -> ())
      ~load_file:(fun ~display:push file -> Reporter.run ~emit:push ~fatal:push @@ fun () -> load_file file)
end

let () =
  match Sys.argv.(1) with
  | "--server" -> Driver.server ()
  | "--github" -> Driver.load `Github Sys.argv.(2)
  | filepath -> Driver.load `Normal filepath
