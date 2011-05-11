structure L1Cbv =
struct

  structure A = Ast


  datatype valuexp = Number of int | Boolean of bool | Abs of A.ident*A.expr
                  
  (* value type. *)
  datatype value = VE of valuexp*(A.ident -> value)
                 | Nil of A.ident -> value
                 | Cons of value*value

  (* raised when an environment applied to id not in domain. *)        
  exception env_error

  (* the empty environment. *)
  val empty_env = fn x => raise env_error

  fun env_update env x v = fn y => if y=x then v else (env y)

  fun value_to_num (VE(Number(n),_)) = n
    | value_to_num _ = raise Fail("Value is not a number")
  fun value_to_bool (VE(Boolean(b),_)) = b
    | value_to_bool _ = raise Fail("Value is not a boolean")
  fun value_to_env (VE(_, env)) = env

  fun eval_closure (e : A.expr) (env : A.ident -> value) =
      case e of
        (A.Ident(i)) => env i
      | (A.Number(n)) => VE(Number(n), empty_env)
      | (A.Boolean(b)) => VE(Boolean(b), empty_env)
      | (A.Abs(i,b)) => VE(Abs(i, b), env)
      | (A.UnOp(rator, e)) =>
        (case rator of
          (A.NEG | A.NOT) =>
          let
            val v = eval_closure e env
          in
            case rator of
              A.NEG => VE(Number(~(value_to_num v)), value_to_env v)
            | A.NOT => VE(Boolean(not(value_to_bool v)), value_to_env v)
          end
        | (A.HEAD | A.TAIL) =>
          let
            val (Cons(v1, v2)) = eval_closure e env
          in
            case rator of
              A.HEAD => v1
            | A.TAIL => v2
          end)
      | (A.BinOp(rator, e1, e2)) =>
        let
          val v1 = eval_closure e1 env
          val v2 = eval_closure e2 env
        in
          case rator of
            A.PLUS  => VE(Number((value_to_num v1)  +  (value_to_num v2)), empty_env)
          | A.SUB   => VE(Number((value_to_num v1)  -  (value_to_num v2)), empty_env)
          | A.TIMES => VE(Number((value_to_num v1)  *  (value_to_num v2)), empty_env)
          | A.DIV   => VE(Number((value_to_num v1) div (value_to_num v2)), empty_env)
          | A.LT    => VE(Boolean((value_to_num v1) <  (value_to_num v2)), empty_env)
          | A.LE    => VE(Boolean((value_to_num v1) <= (value_to_num v2)), empty_env)
          | A.GT    => VE(Boolean((value_to_num v1) >  (value_to_num v2)), empty_env)
          | A.GE    => VE(Boolean((value_to_num v1) >= (value_to_num v2)), empty_env)
          | A.EQ    => VE(Boolean((value_to_num v1) =  (value_to_num v2)), empty_env)
          | A.NE    => VE(Boolean((value_to_num v1) <> (value_to_num v2)), empty_env)
          | A.AND   => VE(Boolean((value_to_bool v1) andalso (value_to_bool v2)), empty_env)
          | A.OR    => VE(Boolean((value_to_bool v1) orelse  (value_to_bool v2)), empty_env)
          | A.CONS  => Cons(v1, v2)
        end
      | A.NilList => Nil(empty_env)
      | (A.Cond(if_e, then_e, else_e)) =>
        let
          val (VE(Boolean(if_v), _)) = eval_closure if_e env
          val then_v = eval_closure then_e env
          val else_v = eval_closure else_e env
        in
          if if_v then then_v else else_v                                       
        end
      | (A.App(rator, rand)) =>
        let
          val (VE(Abs(x,e0), env0)) = eval_closure rator env
          val (VE(v1, env1)) = eval_closure rand env
          val (VE(v', env')) = eval_closure e0 (env_update env0 x (VE(v1, env1)))
        in
          VE(v', env')
        end

  fun value2ast (VE(Number(n),_)) = A.Number(n)
    | value2ast (VE(Boolean(b),_)) = A.Boolean(b)
    | value2ast (VE(Abs(i,e),env)) = A.Abs(i,e) (*A.Abs(i, value2ast VE(e, env)) FIXME *)
    | value2ast (Nil(_)) = A.NilList
    | value2ast (Cons(v1,v2)) = A.BinOp(A.CONS, value2ast v1, value2ast v2)


  fun eval_expr e = eval_closure e empty_env
                                                         


  fun pgm_xfrm (A.Program(A.Assign(x,e)::[])) = e
    | pgm_xfrm (A.Program(A.Assign(x,e)::tl)) = A.App(A.Abs(x, pgm_xfrm (A.Program(tl))), e)
    | pgm_xfrm _ = raise Fail("Not a proper program")
  
  fun eval_pgm p = eval_expr (pgm_xfrm p)
end
