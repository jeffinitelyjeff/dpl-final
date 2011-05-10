signature LEXER =
sig

    val makeLexer : (int -> string) -> (unit -> Tokens.t)

end
