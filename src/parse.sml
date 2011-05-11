(*
 * Need still to do APP
 *
 * 
 *
 *)
structure L1Parse =
struct
  structure T = Tokens
  structure A = Ast

  exception parse_error of string

  datatype expression = E of A.expr | BGROUP

  fun binop2node (T.Binop(x)) e1 e2 = E(A.BinOp(x, e1, e2))
    | binop2node _ _ _ = raise parse_error("Invaid binary operation.")
  fun unop2node (T.Unop(x)) e = E(A.UnOp(x, e))
    | unop2node _ _ = raise parse_error("Invalid unary operation.")
  fun abs2node (T.Lambda(i)) exp = E(A.Abs(i, exp))
    | abs2node _ _ = raise parse_error("Invalid lambda expression")
  fun cond2node if_e then_e else_e = E(A.Cond(if_e, then_e, else_e))

  datatype associativity = LEFT | RIGHT

  (* Returns the correct association rule for the given token.
   * FIXME: Need to somehow make application right-associative. *)
  fun assoc (T.Unop(_) | T.Lambda(_) | T.Binop(A.CONS)) = RIGHT
    | assoc (T.Binop(_)) = LEFT
    | assoc _ = raise parse_error("Association not in grammer")
    
  (* Returns a number representative of the priority of the given operation
   * over others.  Higher numbers denote higher priority.  Useful for 
   * determining order of operations in an expression. *)
   fun prec (T.Lambda(_)) = 0
    | prec (T.Binop(A.OR | A.AND)) = 1                                      
    | prec (T.Binop(A.GT | A.GE | A.LT | A.LE | A.EQ | A.NE)) = 2
    | prec (T.Binop(A.CONS)) = 3 
    | prec (T.Binop(A.PLUS | A.SUB)) = 4
    | prec (T.Binop(A.TIMES | A.DIV)) = 5
    | prec (T.Unop(_)) = 6
    (* | prec (APPLICATION _) = 7 *) (* FIXME *)
    | prec (T.RParen | T.Endif) = ~9000
    | prec _ = raise parse_error("Invalid operator")


  fun parse_expression lexer =
    let
      (* pop_op (e2 :: e1 :: es) (op :: ops) = (((op e1 e2) :: es), ops)
       *       if op is a binary operation,
       *  pop_op (e :: es) (op :: ops) = ((op e) :: es, ops) if op is unary.
       *  Raises parse_error if the expression stack has too few expressions for the
       *  operator or the opstack has no operations.
       *)
      
      fun pop_op estack [] = raise parse_error "Unexpected empty opstack"
        | pop_op [] opstack = raise parse_error "Missing arguments for operator"
        (* Unary operations. *)
        | pop_op (E(rand) :: rands) ((rator as (T.Unop(_))) :: rators) =
            ((unop2node rator rand) :: rands, rators)
        (* Binary operations. *)
        | pop_op (E(rand2)::E(rand1)::rands) ((rator as (T.Binop(_))) :: rators) =
            ((binop2node rator rand1 rand2) :: rands, rators)
        (* Abstraction. *)
        | pop_op (E(rand) :: rands) ((rator as (T.Lambda(_))) :: rators) =
            ((abs2node rator rand) :: rands, rators)
        (* Conditionals. *)
        | pop_op (E(rand3)::E(rand2)::E(rand1)::rands) (T.If :: rators) =
            ((cond2node rand1 rand2 rand3) :: rands, rators)
        | pop_op _ _ = raise parse_error("Invalid op on top of stack")

      and force_ops tok es [] = (es, [])
        | force_ops tok es (T.LParen :: rators) = (es, T.LParen :: rators)
        | force_ops tok es (T.Else   :: rators) = (es, T.Else   :: rators)
        | force_ops tok es (T.Then   :: rators) = (es, T.Then   :: rators)
        | force_ops tok es (T.If     :: rators) = (es, T.If     :: rators)
        | force_ops tok es (rator    :: rators) =
            if prec tok > prec rator orelse
               (prec tok = prec rator andalso assoc tok = RIGHT)
            then (es, (rator :: rators))
            else 
              let
                val (es', rators') = pop_op es (rator :: rators)
              in
                force_ops tok es' rators'
              end

      (* apps_to_bgroup (e1::e2::...::ek::BGROUP::...es...) e0 will return the AST
       * corresponding to ((((ek ek-1)...)e2)e1)e0. *)
      and apps_to_bgroup (BGROUP::_) exp = exp 
        | apps_to_bgroup (E(e)::BGROUP::_) exp = A.App(e, exp)
        | apps_to_bgroup (E(e)::es) exp = apps_to_bgroup es (A.App(e, exp))
        | apps_to_bgroup _ _ = raise parse_error("BGROUP never reached")

      (* after_bgroup (e1::...::ek::BGROUP::...es...) will return all the es after
       * the BGROUP "expression" .*)
      and after_bgroup (BGROUP::es) = es
        | after_bgroup (_::es) = after_bgroup es
        | after_bgroup _ = raise parse_error("No BGROUP found")
              
     (* parse_tokens lexer estack opstack = (es@estack, os@ostack), where 
      *  es is the expression stack and os the operator stack after parsing the
      *  expression that starts with the first token returned by lexer.
      *)
      and parse_tokens lexer estack opstack =
      let
        val tok = lexer()
        val x = print (T.tok2str tok ^ "===")
      in
        case tok of
          (T.Ident(i)) => parse_tokens lexer (E(A.Ident(i))::estack) opstack
        | (T.Num(n))   => parse_tokens lexer (E(A.Number(n))::estack) opstack
        | T.True       => parse_tokens lexer (E(A.Boolean(true))::estack) opstack
        | T.False      => parse_tokens lexer (E(A.Boolean(false))::estack) opstack
        | T.Nil        => parse_tokens lexer (E(A.NilList)::estack) opstack
        | T.If         => parse_tokens lexer estack (T.If :: opstack)
        | T.Then       => parse_tokens lexer estack (T.Then :: opstack)
        | T.Else       => parse_tokens lexer estack (T.Else :: opstack)
        | T.Endif =>
          let
            val ((E(else_e) :: es), (T.Else :: ops)) = force_ops T.Endif estack opstack
            val ((E(then_e) :: es'), (T.Then :: ops')) = force_ops T.Endif es ops
            val ((E(if_e) :: es''), (T.If :: ops'')) = force_ops T.Endif es' ops'
          in
            parse_tokens lexer (E(A.Cond(if_e, then_e, else_e)) :: es'') ops''
          end
        | (T.Unop(_) | T.Binop(_) | T.Lambda(_) | T.Cons) =>
          let
	        val (es', rators') = force_ops tok estack opstack
          in
            parse_tokens lexer es' (tok :: rators')
          end
        | T.LParen => parse_tokens lexer (BGROUP :: estack) (T.LParen :: opstack)
        | T.RParen =>
          let
            val (es, (T.LParen :: rators)) = force_ops T.RParen estack opstack
          in
            case es of
              (e'::BGROUP::es') => parse_tokens lexer (e'::es') rators
            | (E(e')::es') => parse_tokens lexer (E(apps_to_bgroup es' e')::(after_bgroup es')) rators
            | (BGROUP::es') => parse_tokens lexer (after_bgroup es') rators
            | _ => raise parse_error("No BGROUP found on expression stack")
          end
        | (T.EOS) => (estack, opstack)
        | (T.EOF) => raise parse_error("Unexpected end of file")
        | _ => raise parse_error("Statement token invalid")
      end
    in
      let 
        val (es', ops') = parse_tokens lexer [BGROUP] [T.LParen]
        val (es'', [T.LParen]) = force_ops T.RParen es' ops'

        val x = print " "
        fun prin ([] | [BGROUP]) = true
          | prin (E(e)::es) =
            let
              val x = print (A.ast2str e ^ "==")
            in
              prin es
            end
        val b = prin es''
      in
        case es'' of
          [E(e),BGROUP] => e
        | (E(e)::es) => apps_to_bgroup es e
        | _ => raise parse_error("Invalid expression stack returned")
      end
    end

(* This function parses an entire program by taking its statements 
 * and feeding them to the expression parser *)
  fun parse_program lexer = 
    let
      fun parse_lines lexer = 
        let
          val tok = lexer()
        in
          case tok of
            T.Assign(x) => (A.Assign(x,(parse_expression lexer))::parse_lines (lexer))
          | T.EOF => []
          | _ => raise parse_error("invalid beginning of statement")
        end
    in 
      A.Program(parse_lines lexer)
    end
end
