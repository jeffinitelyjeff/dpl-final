(* Signature for structures used by Lexers.  Lexical analyzers
 * produced by ml-lex match this siganture.
 *
 * N. Danner
 * COMP 321.
 *)

signature LEXER =
sig

    (* makeLexer f is a lexer that gets n characters at a time by
     * invoking (f n).
     *)
    val makeLexer : (int -> string) -> (unit -> Tokens.t)

end
