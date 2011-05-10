(* Functor for creating lexers given a structure that matches LEXER.
*  
*  N. Danner
*  COMP 321.
*)

functor Lexers (L : LEXER) =
struct

  (* string_lexer : string -> (unit -> Tokens.t)
  *  string_lexer s is a lexical analyzer that reads the string s
  *  as its character source.
  *)
  fun string_lexer(s) =
  let
    val have_read = ref false ;
    fun read_string(s) =
    let
      val result = if !have_read then "\000" else s
    in
      (have_read := true ; result)
    end
  in
    L.makeLexer(fn n => read_string(s))
  end

  (* file_lexer f : string -> (unit -> Tokens.t)
  *  file_lexer f is a lexical analyzer that reads the file named
  *  by f as its character source.
  *)
  fun file_lexer f =
  let
    val s = TextIO.openIn f
  in
    L.makeLexer(fn n => TextIO.inputN (s, n))
  end

  (* putback_lexer : Tokens.t -> (unit -> Tokens.t) -> (unit -> Tokens.t)
  *  putback_lexer tok lexer is a lexer whose next token is tok, and whose
  *  remaining tokens are yielded by lexer.
  *)
  fun putback_lexer tok lexer =
  let
    val got_tok = ref false ;
    fun next_tok() =
    let
      val result = if !got_tok then tok else lexer()
    in
      (got_tok := true ; result)
    end
  in
    next_tok
  end

end
