structure L1Cbv =
struct

  structure A = Ast

  datatype value = Number of int | Boolean of bool | Abs of A.ident*A.expr

  fun value_to_num (Number(n)) = n
    | value_to_num _ = raise Fail("Value is not a number")
  fun value_to_bool (Boolean(b)) = b
    | value_to_bool _ = raise Fail("Value is not a boolean")
  fun value_to_abs (Abs(i,e)) = (i,e)
    | value_to_abs _ = raise Fail("Value is not an abstraction")

  fun eval_expr (e : A.expr) =
      case e of
        (A.Number(n)) => Number(n)
      | (A.Boolean(b)) => Boolean(b)
      | (A.Abs(i,b)) => Abs(i,b)
      | (A.UnOp(rator, e)) =>
        let
          val v = eval_expr e
        in
          case rator of
            A.NEG => Number((value_to_num v))
          | A.NOT => Boolean(not (value_to_bool v))
          | _ => raise Fail("unary operator not yet considered") (* FIXME *)
        end
      | (A.BinOp(rator, e1, e2)) =>
        let
          val v1 = eval_expr e1
          val v2 = eval_expr e2
        in
          case rator of
            A.PLUS  => Number((value_to_num v1)  +  (value_to_num v2))
          | A.SUB   => Number((value_to_num v1)  -  (value_to_num v2))
          | A.TIMES => Number((value_to_num v1)  *  (value_to_num v2))
          | A.DIV   => Number((value_to_num v1) div (value_to_num v2))
          | A.LT    => Boolean((value_to_num v1) <  (value_to_num v2))
          | A.LE    => Boolean((value_to_num v1) <= (value_to_num v2))
          | A.GT    => Boolean((value_to_num v1) >  (value_to_num v2))
          | A.GE    => Boolean((value_to_num v1) >= (value_to_num v2))
          | A.NE    => Boolean((value_to_num v1) <> (value_to_num v2))
          | A.AND   => Boolean((value_to_bool v1) andalso (value_to_bool v2))
          | A.OR    => Boolean((value_to_bool v1) orelse  (value_to_bool v2))
          | _ => raise Fail("Invalid binary operator")
        end
      (* | A.NilList => *) (* FIXME *)
      | (A.Cond(if_e, then_e, else_e)) =>
        let
          val (Boolean(if_v))   = eval_expr if_e
          val (Boolean(then_v)) = eval_expr then_e
          val (Boolean(else_v)) = eval_expr else_e
        in
          if if_v then Boolean(then_v) else Boolean(else_v)
        end
      (* | (A.App(rator, rand)) => *) (* FIXME *)
      | _ => raise Fail("expression not yet considered") (* FIXME *)

  fun value2ast (Number(n)) = A.Number(n)
    | value2ast (Boolean(b)) = A.Boolean(b)
    | value2ast (Abs(i,e)) = A.Abs(i,e)
(*    | values2ast _ = raise Fail("not a value considered yet") (* FIXME *) *)


                                                         

 (* THISISFORCLOSURES
  type valenv = A.ident -> value

  datatype value = Number of int*valenv
                 | Bool of bool*valenv
                 | Abs of A.ident*A.expr*valenv
                 | Nil of valenv
                 | Cons of value*value
  datatype closure = Closure of A.expr*valenv

  fun env_update env x v =
    case env of
      valenv => (fn y => if y=x then v else (env y))
      | _ => raise Fail("Update env called on non-environment")
                           
  val empty_env = fn _ => raise Fail("Accessing an empty environment")
                           
  fun eval_clos (Closure(A.Number(n), _)) = Number(n, empty_env)
    | eval_clos (Closure(A.Boolean(b), _)) = Bool(b, empty_env)
    | eval_clos (Closure(A.UnOp(rator, e), env)) =
      let
        fun apply_unrator A.NEG e = ~e
          | apply_unrator A.NOT e = not e
          | apply_unrator A.HEAD e = hd e
          | apply_unrator A.TAIL e = tl e
          | apply_unrator _ _ = raise Fail("Not a unary operator")
      in
        case rator of
          A.NEG => Number(apply_unrator rator (eval_clos Closure(e, env)))
          | A.NOT => Bool(apply_unrator rator (eval_clos Closure(e, env)))
          | A.HEAD => raise Fail("FIXME")
          | A.TAIL => raise Fail("FIXME")
(* FIXME *)
      end
*)

  fun pgm_xfrm (A.Program(A.Assign(x,e)::[])) = e
    | pgm_xfrm (A.Program(A.Assign(x,e)::tl)) = A.App(A.Abs(x, pgm_xfrm (A.Program(tl))), e)
    | pgm_xfrm _ = raise Fail("Not a proper program")
  
  fun eval_pgm p = raise Fail("")(*eval_expr (pgm_xfrm p)*)
(* THISISFORCLOSURES
  fun values2ast (Number(n,_)) = A.Number(n)
    | values2ast (Boolean(b,_)) = A.Boolean(b)
    | values2ast (Abs(i,e,env)) = raise Fail("FIXME")(* FIXME, this needs to actually go in and read
                                 the environment and fill in values. lame. *)
    | values2ast (Nil(_)) = A.NilList
    | values2ast (Cons(v0,v1)) = A.BinOp(CONS, values2ast v0, values2ast v1)
    | values2ast _ = raise Fail("Not a proper value; can't convert to AST")
*)
end
