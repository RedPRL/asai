let lift_syslib f =
  Logger.adopt (Asai.Diagnostic.map (fun c -> Logger.Code.Syslib c)) Syslib.Logger.run f

module Term = Asai.Tty.Make (Logger.Code)

let () =
  Logger.run ~emit:Term.interactive_trace ~fatal:Term.display @@ fun () ->
  (lift_syslib @@ fun () -> Syslib.Operations.operation1 "op1");
  (lift_syslib @@ fun () -> Syslib.Operations.operation2 "op2");
  lift_syslib @@ fun () -> Syslib.Operations.operation3 "op3"
