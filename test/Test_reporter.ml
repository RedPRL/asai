module Message = struct
  type t = int
  let default_severity _ = Asai.Diagnostic.Error
  let short_code = string_of_int
  let default_text n = Asai.Text.make (string_of_int n)
end

module Reporter = Asai.Reporter.Make(Message)
module Structured = Asai.Structured_reporter.Make(Message)

module Tests (R : sig
  val emit_diagnostic : int Asai.Diagnostic.t -> unit
  val fatal_diagnostic : int Asai.Diagnostic.t -> 'a
  val try_with : ?emit:(int Asai.Diagnostic.t -> unit) -> ?fatal:(int Asai.Diagnostic.t -> 'a) -> (unit -> 'a) -> 'a
end) = struct
  let diagnostic = Asai.Diagnostic.make Error 7 "test"
  let check = Alcotest.(check int)
  let recover _ = 42

  let cases = [
    Alcotest.test_case "normal return" `Quick (fun () ->
      check "result" 7 (R.try_with (fun () -> 7)));
    Alcotest.test_case "fatal recovery" `Quick (fun () ->
      check "result" 42 (R.try_with ~fatal:recover (fun () -> R.fatal_diagnostic diagnostic)));
    Alcotest.test_case "default fatal propagation" `Quick (fun () ->
      check "outer recovery" 42 (R.try_with ~fatal:recover (fun () ->
        R.try_with (fun () -> R.fatal_diagnostic diagnostic))));
    Alcotest.test_case "fatal callback escapes" `Quick (fun () ->
      check "outer recovery" 42 (R.try_with ~fatal:recover (fun () ->
        R.try_with ~fatal:R.fatal_diagnostic (fun () -> R.fatal_diagnostic diagnostic))));
    Alcotest.test_case "unrelated exception propagates" `Quick (fun () ->
      Alcotest.check_raises "Exit" Exit (fun () ->
        ignore (R.try_with ~fatal:recover (fun () -> raise Exit))));
    Alcotest.test_case "omitted emit reaches outer handler" `Quick (fun () ->
      let seen = ref [] in
      let result = R.try_with ~emit:(fun d -> seen := d.Asai.Diagnostic.message :: !seen) (fun () ->
        R.try_with ~fatal:recover (fun () ->
          R.emit_diagnostic diagnostic;
          R.emit_diagnostic diagnostic;
          9)) in
      check "result" 9 result;
      Alcotest.(check (list int)) "diagnostics" [7; 7] !seen);
    Alcotest.test_case "explicit emit intercepts" `Quick (fun () ->
      let count = ref 0 in
      R.try_with ~emit:(fun _ -> Alcotest.fail "unexpected outer emit") (fun () ->
        R.try_with ~emit:(fun _ -> incr count) (fun () ->
          R.emit_diagnostic diagnostic;
          R.emit_diagnostic diagnostic));
      check "handled twice" 2 !count);
    Alcotest.test_case "emit exception unwinds continuation" `Quick (fun () ->
      let cleaned = ref false in
      let result = R.try_with ~emit:R.fatal_diagnostic ~fatal:recover (fun () ->
        Fun.protect ~finally:(fun () -> cleaned := true) (fun () ->
          R.emit_diagnostic diagnostic; 0)) in
      check "recovered" 42 result;
      Alcotest.(check bool) "cleanup ran" true !cleaned);
    Alcotest.test_case "fatal-only allocation" `Quick (fun () ->
      (* Compare the public fatal-only path with explicit forwarding under
         the same runtime. This catches reintroducing the redundant handler
         without inspecting private representation or timing execution.
         Require a substantial reduction so optional-argument allocation
         alone cannot satisfy the check. It measures this small workload,
         not application-wide performance. *)
      let body () = Sys.opaque_identity 7 in
      let allocated f =
        let before = Gc.allocated_bytes () in
        for _ = 1 to 10000 do ignore (Sys.opaque_identity (f ())) done;
        Gc.allocated_bytes () -. before
      in
      let omitted = allocated (fun () -> R.try_with ~fatal:recover body) in
      let explicit = allocated (fun () -> R.try_with ~emit:R.emit_diagnostic ~fatal:recover body) in
      if omitted *. 2. >= explicit then
        Alcotest.failf "expected less than half the allocation without emit: omitted=%g explicit=%g" omitted explicit);
  ]
end

module Free_tests = Tests(Reporter)
module Structured_tests = Tests(Structured)

let () = Alcotest.run "Reporter" [
  "free-form", Free_tests.cases;
  "structured", Structured_tests.cases;
]
