structure Typing = 
struct

  structure A = Ast

  exception infer_error
            
  (* this is not ideal, but we were pressed for time not think of the clever way
   * to store type variables. *)
  type var = int

  datatype t = V of var | Int | Bool | Arrow of t*t | List of t

  (* labeled AST. each node has a var component. *)
  datatype lexpr = LIdent of A.ident*var
                 | LNumber of int*var
                 | LBoolean of bool*var
                 | LUnOp of A.unop*lexpr*var
                 | LBinOp of A.binop*lexpr*lexpr*var
                 | LNilList of var
                 | LCond of lexpr*lexpr*lexpr*var
                 | LAbs of A.ident*var*lexpr*var
                 | LApp of lexpr*lexpr*var

  (* This is a type value environment, which maps identifiers to type variables.
   * Type variables happen to be integers. 0 is not a valid type variable value,
   * so this empty environment, when applied to anything will return an invalid
   * variable value (which we can easily test for, conveniently). *)
  val empty_env = fn x => 0
                          
  fun env_add env x v = fn y => if y=x then v else (env y)

  fun label_ast x env (A.Ident(i)) = LIdent(i, env i)
    | label_ast x env (A.Number(n)) = LNumber(n,x)
    | label_ast x env (A.Boolean(b)) = LBoolean(b,x)
    | label_ast x env (A.UnOp(rator, rand)) = LUnOp(rator,
                                                    label_ast (x+1) env rand, x)
    | label_ast x env (A.BinOp(rator, e1, e2)) = LBinOp(rator,
                                                        label_ast (x+1) env e1,
                                                        label_ast (x+2) env e2,
                                                        x)
    | label_ast x env (A.NilList) = LNilList(x)
    | label_ast x env (A.Cond(e0,e1,e2)) = LCond(label_ast (x+1) env e0,
                                                 label_ast (x+2) env e1,
                                                 label_ast (x+3) env e2, x)
    | label_ast x env (A.Abs(i,e)) = LAbs(i, x+1, 
                                     label_ast (x+2) (env_add env i (x+1)) e,
                                     x)
    | label_ast x env (A.App(e0,e1)) = LApp(label_ast (x+1) env e0,
                                            label_ast (x+2) env e1, x)

(*  fun gen_constraints cs LIdent(i, x) = *)
            

  (* infer e => the most general type of e, inferred using Hindley-Milner.
   *  Raises infer_error if no type can be inferred for e.
   *)
  fun infer e = raise Fail("Need to implement type inference")


  (* toString s => a fully-parenthesized string representation of the
   *  type s.
   *
   * val toString : t -> string
   *)
  fun toString (V(v)) = chr(v)
    | toString (Int) = "int"
    | toString (Bool) = "bool"
    | toString (Arrow(e1, e2) = "("^(toString e1)^" --> "^(toString e2)^")"
    | toString (List(t)) = (toString t)^" list"
(*    | toString _ = raise infer_error("invalid type") *)
end
