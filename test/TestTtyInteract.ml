open Asai

let width = 10

let s title : Range.source =
  `String
    { title = title
    ; content = String.concat "\n" (List.map (String.make width) ['a';'b';'c';'d';'e'])
    }

let s1 = s None
let s2 = s (Some "test")

let (~@) source line col : Range.position =
  { source
  ; offset = (line-1) * (width+1) + col
  ; start_of_line = (line-1) * (width+1)
  ; line_num = line}

module Reporter =
struct
  module Code =
  struct
    type t = Hello | Bye
    let default_severity : t -> Asai.Diagnostic.severity =
      function
      | Hello -> Warning
      | Bye -> Error
    let short_code : t -> string =
      function
      | Hello -> "hello"
      | Bye -> "bye"
  end

  include Asai.Reporter.Make(Code)
end

module Terminal = Asai.Tty.Make(Reporter.Code)

let () =
  Reporter.run
    ~emit:Terminal.interact ~fatal:Terminal.interact @@ fun () ->
  Reporter.emitf Hello "aloha %d" 100;
  Reporter.emitf Bye "aloha %d" 200;
  Reporter.emit ~loc:(Range.make (~@ s1 1 3, ~@ s1 2 4)) Hello "hello here!";
  Reporter.emit ~loc:(Range.make (~@ s2 2 3, ~@ s2 3 5)) Bye "bye there!";
  Reporter.emit ~loc:(Range.make (~@ s1 1 3, ~@ s1 2 4)) Hello "this is a bug" ~severity:Bug;
  Reporter.emit ~loc:(Range.make (~@ s1 1 3, ~@ s1 2 4)) Hello "this is an error" ~severity:Error;
  Reporter.emit ~loc:(Range.make (~@ s1 1 3, ~@ s1 2 4)) Hello "this is a warning" ~severity:Warning;
  Reporter.emit ~loc:(Range.make (~@ s1 1 3, ~@ s1 2 4)) Hello "this is an info" ~severity:Info;
  Reporter.emit ~loc:(Range.make (~@ s1 1 3, ~@ s1 2 4)) Hello "this is a hint" ~severity:Hint;
  Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "this is a bug" ~severity:Bug;
  Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "this is an error" ~severity:Error;
  Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "this is a warning" ~severity:Warning;
  Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "this is an info" ~severity:Info;
  Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "this is a hint" ~severity:Hint;

  Reporter.trace "I'm a frame" begin fun () ->
    Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "Hello again!";
    Reporter.trace "I'm another frame" @@ fun () ->
    begin
      Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "Hello once again!";
    end
  end;

  Reporter.trace ~loc:(Range.make (~@ s2 1 1, ~@ s2 3 4)) "I'm a frame" begin fun () ->
    Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "Hello again!";
    Reporter.trace "I'm another frame" begin fun () ->
      Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "Hello once again!" ~severity:Info;
    end
  end;

  Reporter.emit ~loc:(Range.make (~@ s1 2 3, ~@ s1 2 7)) Hello "hello here!"
    ~extra_remarks:[
      Diagnostic.loctext "Message 1";
      Diagnostic.loctext ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) "Message 2";
      Diagnostic.loctext "Message 3";
      Diagnostic.loctext ~loc:(Range.make (~@ s1 3 4, ~@ s1 3 5)) "Message 4";
      Diagnostic.loctext ~loc:(Range.make (~@ s1 2 8, ~@ s1 2 9)) "Message 5";
      Diagnostic.loctext ~loc:(Range.make (~@ s1 1 3, ~@ s1 2 1)) "Message 6";
      Diagnostic.loctext ~loc:(Range.make (~@ s2 1 3, ~@ s2 2 1)) "Message 7";
    ];
