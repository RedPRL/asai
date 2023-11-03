open Asai


let width = 10
let template = "abcdefghijklmnopqrstuvw"

let s title : Range.source =
  `String
    { title = title
    ; content =
        String.concat "\n" @@
        List.init  (String.length template) @@ fun i ->
        String.make width template.[i]
    }

let s1 = s None
let s2 = s (Some "/path/to/file.cool")

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

let exec handler =
  Reporter.run ~emit:handler ~fatal:handler @@ fun () ->
  Reporter.emit Hello "this is a warning\nhaving two lines" ~severity:Warning;
  Reporter.emit Bye "this is an error\nhaving two lines" ~severity:Error;
  Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "this is a bug" ~severity:Bug;
  Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "this is an error" ~severity:Error;
  Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "this is a warning" ~severity:Warning;
  Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "this is an info" ~severity:Info;
  Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "this is a hint" ~severity:Hint;

  Reporter.trace "when peaking into the abyss" begin fun () ->
    Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "hello from here";
    Reporter.trace "when peaking into the deep abyss" @@ fun () ->
    begin
      Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "hello from here";
    end
  end;

  Reporter.trace ~loc:(Range.make (~@ s2 1 1, ~@ s2 3 4)) "when stepping into the abyss" begin fun () ->
    Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "hello from here";
    Reporter.trace ~loc:(Range.make (~@ s2 1 1, ~@ s2 3 4)) "when stepping into the deep abyss" begin fun () ->
      Reporter.emit ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) Hello "hello from here" ~severity:Info;
    end
  end;

  Reporter.emit ~loc:(Range.make (~@ s1 2 3, ~@ s1 2 7)) Hello "this is the main message"
    ~extra_remarks:[
      Diagnostic.loctext "message 1";
      Diagnostic.loctext ~loc:(Range.make (~@ s2 1 3, ~@ s2 3 4)) "message 2";
      Diagnostic.loctext "message 3";
      Diagnostic.loctext ~loc:(Range.make (~@ s1 3 4, ~@ s1 3 5)) "message 4";
      Diagnostic.loctext ~loc:(Range.make (~@ s1 2 8, ~@ s1 2 9)) "message 5";
      Diagnostic.loctext ~loc:(Range.make (~@ s1 1 3, ~@ s1 2 1)) "message 6";
      Diagnostic.loctext ~loc:(Range.make (~@ s2 1 3, ~@ s2 2 1)) "message 7";
      Diagnostic.loctext ~loc:(Range.make (~@ s2 10 0, ~@ s2 10 0)) "message 8";
    ];

  Reporter.emit ~loc:(Range.eof (~@ s1 23 width)) Hello "this is the main message"
    ~extra_remarks:[
      Diagnostic.loctext ~loc:(Range.eof (~@ s2 23 width)) "ending of another file";
    ]

let () =
  exec (Terminal.display ~use_ansi:true ~use_color:true);
  exec (Terminal.display ~use_ansi:true ~use_color:false);
  exec (Terminal.display ~use_ansi:false ~use_color:false);
