let lift_syslib f =
  Reporter.adopt (Asai.Diagnostic.map (fun c -> Reporter.Message.Syslib c)) Syslib.Reporter.run f

module Term = Asai.Tty.Make(Reporter.Message)

let () =
  Reporter.run ~emit:Term.display ~fatal:Term.display @@ fun () ->
  (lift_syslib @@ fun () -> Syslib.Operations.operation1 "op1");
  (lift_syslib @@ fun () -> Syslib.Operations.operation2 "op2");
  lift_syslib @@ fun () -> Syslib.Operations.operation3 "op3"
