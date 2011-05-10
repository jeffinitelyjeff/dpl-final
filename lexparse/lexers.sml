functor Lexers (L : LEXER) =
struct

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

  fun file_lexer f =
    L.makeLexer(fn n => (print (Int.toString(n) ^ "\n") ;TextIO.inputN
    (TextIO.openIn f, n)))

end
