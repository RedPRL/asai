let operation1 arg =
  Reporter.emitf ChiError "I got a string %s" arg

let operation2 arg =
  Reporter.trace "Meow!\nMeow!" @@ fun () ->
  operation1 arg

let operation3 arg =
  Reporter.fatalf EmojiError "Not enough emojis in the string %s" (String.escaped arg)
