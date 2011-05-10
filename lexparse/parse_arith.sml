structure Parse =
struct

  structure T = Tokens
  structure A = Ast

  fun binop2node T.Plus = A.Plus
    | binop2node T.Times = A.Times
    | binop2node _ = raise Fail("Invalid binary operator")

  fun unop2node T.Neg = A.Neg
    | unop2node _ = raise Fail("Invalid unary operator")

  datatype associativity = LEFT | RIGHT

  fun assoc (T.Plus | T.Times) = LEFT
    | assoc (T.Neg) = RIGHT

  fun prec T.Plus = 10
    | prec T.Times = 20
    | prec T.Neg = 9000
    | prec T.RParen = ~9999
    | prec _ = raise Fail("Invalid operator")

  fun parse lexer =
  let

    (* pop_op (e2 :: e1 :: es) (op :: ops) = (((op e1 e2) :: es), ops)
    *       if op is a binary operation.
    *  pop_op (e :: es) (op :: ops) = ((op e) :: es, ops) if op is unary.
    *  Raises Fail if the expression stack has too few expressions for the
    *  operator or the opstack has no operations.
    *)
    fun pop_op estack [] = raise Fail "Unexpected empty opstack"
      | pop_op [] opstack = raise Fail "Missing arguments for operator"
      | pop_op (rand :: rands) 
               ((rator as (T.Neg)):: rators) =
            (((unop2node rator)(rand) :: rands), rators)
      | pop_op (rand2 :: rand1 :: rands) 
               ((rator as (T.Plus | T.Times)):: rators) =
            (((binop2node rator)(rand1, rand2) :: rands), rators)

    and force_ops tok es [] = (es, [])
      | force_ops tok es (T.LParen :: rators) = (es, T.LParen :: rators)
      | force_ops tok es (rator :: rators) =
            if prec tok > prec rator orelse
               (prec tok = prec rator andalso assoc tok = RIGHT)
            then (es, (rator :: rators))
            else 
              let
                val (es', rators') = pop_op es (rator :: rators)
              in
                force_ops tok es' rators'
              end

    (* parse_tokens lexer estack opstack = (es@estack, os@ostack), where 
    *  es is the expression stack and os the operator stack after parsing the
    *  expression that starts with the first token returned by lexer.
    *)
    and parse_tokens lexer estack opstack =
    let
      val tok = lexer()
      (* val x = print (T.toString tok ^ "===") *)
    in
      case tok of
           T.Number(n) => parse_tokens lexer (A.Number(n)::estack) opstack
         | (T.Neg | T.Plus | T.Times) =>
             let
               val (es', rators') = force_ops tok estack opstack
             in
               parse_tokens lexer es' (tok :: rators')
             end
         | T.LParen => parse_tokens lexer estack (tok :: opstack)
         | T.RParen =>
             let
               val (es', (T.LParen :: rators)) = force_ops tok estack opstack
             in
               parse_tokens lexer es' rators
             end
         | (T.EOF | T.EOS) => (estack, opstack)
    end
  in
    let 
      val (es', ops') = parse_tokens lexer [] [T.LParen]
      val ([e], [T.LParen]) = force_ops T.RParen es' ops'
    in
      e
    end
  end

end
