structure Tokens =
struct

  datatype t = Number of int | Neg | Plus | Times | LParen | RParen | EOF  | EOS

  fun toString (Number(n)) = Int.toString(n)
    | toString Neg = "~"
    | toString Plus = "+"
    | toString Times = "*"
    | toString LParen = "("
    | toString RParen = ")"

    | toString EOF = "<eof>"
    | toString EOS = "<eos>"

end
