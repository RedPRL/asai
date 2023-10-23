let operation1 arg =
  Reporter.emitf ChiError "got the string `%s'" (String.escaped arg)

let operation2 arg =
  Reporter.trace "meow\nmeow" @@ fun () ->
  operation1 arg

let operation3 arg =
  Reporter.fatalf EmojiError "not enough emojis in the string %s" (String.escaped arg)
