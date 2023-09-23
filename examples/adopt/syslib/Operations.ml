let operation1 arg =
  Logger.emitf ChiError "I got a string %s" arg

let operation2 arg =
  Logger.trace_string "Meow!\nMeow!" @@ fun () ->
  operation1 arg

let operation3 arg =
  Logger.fatalf EmojiError "Not enough emojis in the string %s" (String.escaped arg)
