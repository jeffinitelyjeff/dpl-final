signature TYPING = 
sig

    (* Raised by infer if the expression cannot be typed. *)
    exception infer_error ;

    (* The type of type variables. *)
    eqtype var ;

    (* The type of types. *)
    datatype t = V of var | Int | Bool | Arrow of t*t | List of t

    (* infer e => the most general type of e, inferred using Hindley-Milner.
    *  Raises infer_error if no type can be inferred for e.
    *)
    val infer : Ast.expr -> t ;

    (* toString s => a fully-parenthesized string representation of the
    *  type s.
    *)
    val toString : t -> string ;

end
