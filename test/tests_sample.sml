structure Tests =
struct

  structure Ast = Ast
  structure Lexers = Lexers(L1Lex)

  structure L1Parse = L1Parse : PARSE
  open L1Parse

  structure L1Cbv = L1Cbv : EVAL
  open L1Cbv

  structure Typing = Typing :> TYPING
  open Typing

  structure U = UnitTest

  (* An environment maps an equality type to a type. *)
  type (''a,'b) env = ''a -> 'b

  (* Raised when an environment is applied to an identifier not in its domain.
  *)
  exception env_error ;

  (* The empty environment. *)
  val env_empty = (fn v => raise env_error) : (''a,'b) env

  (* env_update (env, id, v) = env', where env' = [v/id]env. *)
  fun env_update(e, id, v) = fn x => if x = id then v else e(x) ;

  (* env_get (env, id) = env(id) (i.e., the value id is mapped to by env. *)
  fun env_get(e, id) = e(id) ;

  (* Testing types to parallel TYPING.t.  We need this so that we have
  *  no dependence on how Typing.t is implemented.  We will convert any
  *  type returned by Typing.infer to a ttype, and then verify that the
  *  expected type and the converted type are renamings of each other.
  *)
  type tvar = int
  datatype ttype = tV of tvar | tInt | tBool | tArrow of ttype*ttype |
    tList of ttype

  (* ttotype (sigma : TYPING.t) : ttype
  *  ttotype sigma = sigma', where sigma' is a ttype with the same
  *  structure as sigma, but with TYPING.var leaves replaced with
  *  ttype leaves.
  *)
  fun tottype sigma =
  let
    fun rename (V x) so_far fresh =
    let
      val tx = (env_get(so_far, x)) handle env_error => fresh
    in
      ((tV tx), env_update(so_far, x, tx), 
        if tx < fresh then fresh else fresh + 1)
    end
      | rename Int so_far fresh = (tInt, so_far, fresh)
      | rename Bool so_far fresh = (tBool, so_far, fresh)
      | rename (Arrow(sigma, tau)) so_far fresh =
        let
          val (sigma', so_far', fresh') = rename sigma so_far fresh
          val (tau', so_far'', fresh'') = rename tau so_far' fresh'
        in
          (tArrow(sigma', tau'), so_far'', fresh'')
        end
      | rename (List sigma) so_far fresh =
        let
          val (sigma', so_far', fresh') = rename sigma so_far fresh
        in
          (tList(sigma'), so_far', fresh')
        end
  in
    #1(rename sigma env_empty 0)
  end

  (* ttoString (sigma : ttype) : string
   * ttoString (sigma : ttype) is a string representation of sigma.
   *)
  fun ttoString (tV(i)) =
    if i < 26 then Char.toString(Char.chr(97+i))
    else if i < 52 then Char.toString(Char.chr(64+(i-26)))
         else "(a{" ^ Int.toString(i-52) ^ "})"
    | ttoString (tInt) = "int"
    | ttoString (tBool) = "int"
    | ttoString (tArrow(rho, tau)) = "(" ^ ttoString(rho) ^ ") -> (" ^ 
                                        ttoString(tau) ^ ")"
    | ttoString (tList(sigma)) = "[" ^ ttoString(sigma) ^ "]"

  (* is_renaming (sigma : ttype) (tau : ttype) : bool
  *  is_renaming sigma tau = true if sigma and tau are renamings of each
  *  other, false otherwise.
  *)
  fun is_renaming sigma tau =
  let
    fun get_renaming (tV(i)) (tV(j)) so_far =
    let
      val (tV curr_bind) = (env_get(so_far, i)) handle env_error => (tV j)
    in
      if curr_bind = j then env_update(so_far, i, (tV j))
      else raise Fail ""
    end
      | get_renaming tInt tInt so_far = so_far
      | get_renaming tBool tBool so_far = so_far
      | get_renaming (tArrow(sigma, tau)) (tArrow(sigma', tau')) so_far =
          let
            val left = get_renaming sigma sigma' so_far
            val right = get_renaming tau tau' left
          in
            right
          end
      | get_renaming (tList sigma) (tList sigma') so_far =
            get_renaming sigma sigma' so_far
      | get_renaming _ _ _ = raise Fail ""
    val renaming_st = get_renaming sigma tau env_empty
    val renaming_ts = get_renaming tau sigma env_empty
  in
    true
  end
  handle Fail(_) => false

  (* A string lexer. *)
  val sl = fn s => Lexers.string_lexer s
  (* A file lexer. *)
  val fl = fn f => Lexers.file_lexer f

  (* Test whether test() = exp : Ast.expr. *)
  fun do_test_ast(name, test, exp) = 
    U.do_test(name, fn () => parse_expression (sl test), exp, Ast.ast2str)

  (* Test whether EVAL.value2ast(test()) = EVAL.value2ast(exp). *)
  fun do_test_eval(name, test, exp) =
    U.do_test(name, 
              fn () => value2ast (eval_expr (
                            parse_expression (sl test))), exp, Ast.ast2str)

  (* Test whether the program named by filename evaluates to exp : Ast.expr. *)
  fun do_test_file_eval filename exp =
    U.do_test(filename, 
              fn () => value2ast (eval_pgm (
                          parse_program (fl ("test_files/" ^ filename)))),
              exp,
              Ast.ast2str)

  (* Test whether the correct type is inferred for the provided expression. *)
  fun do_test_type(name, test, exp) =
    U.do_test_eq(name,
                 fn () => tottype (infer (parse_expression (sl test))),
                 exp,
                 is_renaming,
                 ttoString)

  (* Parsing tests. *)
  val test_ast = fn () => (

    (* "Arithemtic" expressions with precedence and parentheses. *)
    do_test_ast("Ident1", "x ;" , Ast.Ident("x")) ;
    do_test_ast("Ident2", "xyz ;", Ast.Ident("xyz")) ;
    do_test_ast("Num. const.", "5 ;", Ast.Number(5)) ;

    do_test_ast("Bool1", "x andalso y;",
      Ast.BinOp(Ast.AND, Ast.Ident "x", Ast.Ident "y")) ;
    do_test_ast("Bool4", "not x andalso y;",
      Ast.BinOp(Ast.AND, Ast.UnOp(Ast.NOT, Ast.Ident "x"), Ast.Ident "y")) ;
    do_test_ast("Bool5", "x < y orelse z < y;",
      Ast.BinOp(Ast.OR,
                Ast.BinOp(Ast.LT, Ast.Ident "x", Ast.Ident "y"),
                Ast.BinOp(Ast.LT, Ast.Ident "z", Ast.Ident "y"))) ;

    do_test_ast("List 1", "[];", Ast.NilList) ;
    do_test_ast("List 2", "2 :: 3 :: [];",
      Ast.BinOp(Ast.CONS, Ast.Number 2,
                   Ast.BinOp(Ast.CONS, Ast.Number 3, Ast.NilList))) ;
    do_test_ast("List 5", "5 + 3 :: [];", 
      Ast.BinOp(Ast.CONS,
                Ast.BinOp(Ast.PLUS, Ast.Number 5, Ast.Number 3),
                Ast.NilList)) ;

    do_test_ast("List 7", "((fn x => fn y => x + y)(2)) :: [];",
      Ast.BinOp(Ast.CONS,
                Ast.App(Ast.Abs("x",
                                Ast.Abs("y",
                                        Ast.BinOp(Ast.PLUS, 
                                                  Ast.Ident "x",
                                                  Ast.Ident "y"))),
                        Ast.Number 2),
                Ast.NilList)) ;

    do_test_ast("Arith .9", "3 + 5;", 
        Ast.BinOp(Ast.PLUS, Ast.Number(3), Ast.Number(5))) ;

    do_test_ast("Arith 1", "x * 3 + 4 / 5;",
      Ast.BinOp(Ast.PLUS, Ast.BinOp(Ast.TIMES, Ast.Ident("x"), Ast.Number(3)),
      Ast.BinOp(Ast.DIV, Ast.Number(4),
      Ast.Number(5)))) ;
    do_test_ast("Arith 3", "x * ((3+4)/5);",
      Ast.BinOp(Ast.TIMES,
                Ast.Ident("x"),
                Ast.BinOp(Ast.DIV,
                          Ast.BinOp(Ast.PLUS,
                                    Ast.Number 3, Ast.Number 4),
                          Ast.Number 5))) ;

    do_test_ast("NEG 1", "~3 ;", 
      Ast.UnOp(Ast.NEG, Ast.Number(3))) ;
    do_test_ast("NEG 2", "~(2+5) ;", 
      Ast.UnOp(Ast.NEG,
               Ast.BinOp(Ast.PLUS, Ast.Number(2), Ast.Number(5)))) ;

    do_test_ast("NumRel2", "x+y >= z-y;",
      Ast.BinOp(Ast.GE,
                Ast.BinOp(Ast.PLUS, Ast.Ident "x", Ast.Ident "y"),
                Ast.BinOp(Ast.SUB, Ast.Ident "z", Ast.Ident "y"))) ;

    (* Abstraction *)
    do_test_ast("ParenAbs", "fn x => (x);", Ast.Abs("x", Ast.Ident("x"))) ;
    do_test_ast("NoParenAbs", "fn x => x;", Ast.Abs("x", Ast.Ident("x"))) ;
    do_test_ast("Abs1", "fn x => x + 3;", Ast.Abs("x",
      Ast.BinOp(Ast.PLUS, Ast.Ident("x"), Ast.Number(3)))) ;
    do_test_ast("Abs2", "fn x => fn y => x + y;",
      Ast.Abs("x", 
              Ast.Abs("y", 
                      Ast.BinOp(Ast.PLUS,
                                Ast.Ident("x"), Ast.Ident("y"))))) ;

    (* Conditionals *)
    do_test_ast("Cond2", "if 5 > 7 then 12 else zed fi;",
        Ast.Cond(Ast.BinOp(Ast.GT, Ast.Number(5), Ast.Number(7)), 
                 Ast.Number(12), 
                 Ast.Ident("zed"))) ;

    (* Fully-parenthesized abstraction and application. *)
    do_test_ast("ParenApp 1", "(x)(7);", 
        Ast.App(Ast.Ident("x"), Ast.Number(7))) ;

    do_test_ast("ParenApp 2", "(fn x => (x + 3))(7);",
        Ast.App(Ast.Abs("x", Ast.BinOp(Ast.PLUS, Ast.Ident("x"),
        Ast.Number(3))), Ast.Number(7))) ;

    (* Not fully-parenthesized abstraction and application. *)
    do_test_ast("UnParenApp 1", "x 3;",
        Ast.App(Ast.Ident "x",
                Ast.Number 3)) ;
    do_test_ast("UnParenApp 2", "x 3 5;",
      Ast.App(Ast.App(Ast.Ident "x",
                      Ast.Number 3),
              Ast.Number 5)) ;
    do_test_ast("App 5", "(fn x => x) 3 + 4 ;",
      Ast.BinOp(Ast.PLUS,
                Ast.App(Ast.Abs("x", Ast.Ident "x"), Ast.Number 3), 
                Ast.Number 4)) ;

    true
  )

  val test_eval = fn () => (
    do_test_eval("Eval num 1", "5;", Ast.Number(5)) ;
    do_test_eval("Eval true", "true;", Ast.Boolean(true)) ;
    do_test_eval("Eval arith 2", "2 - 2;", Ast.Number(0)) ;
    do_test_eval("Eval cond. 2", "if false then 0 else 1 fi;", Ast.Number(1)) ;
    do_test_eval("Eval cond. 3", "if 3 <= 5 then 0 else 1 fi;", Ast.Number(0)) ;

    do_test_eval("Eval list 1", "[];", Ast.NilList) ;
    do_test_eval("Eval list 2", "1 :: [];", 
        Ast.BinOp(Ast.CONS, Ast.Number(1), Ast.NilList)) ;
    do_test_eval("Eval list 3", "1 :: 2 :: [];",
        Ast.BinOp(Ast.CONS, 
                  Ast.Number(1), 
                  Ast.BinOp(Ast.CONS, Ast.Number(2), Ast.NilList))) ;
    do_test_eval("Eval list 5", "((fn x => fn y => x + y)(2)) :: [];",
        Ast.BinOp(Ast.CONS, Ast.Abs("y", 
                           Ast.BinOp(Ast.PLUS, Ast.Number(2), Ast.Ident("y"))),
                           Ast.NilList)) ;
                    
    do_test_eval("Eval hd/tl 3", "hd tl (1 :: 2 :: []);", Ast.Number(2)) ;

    do_test_eval("Eval abs 1", "fn x => x;", 
        Ast.Abs("x", Ast.Ident("x"))) ;
    do_test_eval("Eval abs 3", "(fn x => fn y => x + y)(2);",
        Ast.Abs("y", Ast.BinOp(Ast.PLUS, Ast.Number(2), Ast.Ident("y")))) ;

    do_test_eval("Eval app 3", "(fn x => x)(fn y => y + y);",
        Ast.Abs("y", Ast.BinOp(Ast.PLUS, Ast.Ident("y"),
        Ast.Ident("y")))) ;

    true
  )

  fun test_eval_file() = 
  let
    val files_results = [
        ("t3.l1", Ast.Number 8)
    ]
    fun test_files [] = true
      | test_files ((test, res) :: frs) = 
            (do_test_file_eval test res ; test_files frs)
  in
    test_files files_results
  end

  fun test_typing () = (
    do_test_type("Pure type 1", "fn x => x ;", tArrow(tV 0, tV 0)) ;
    do_test_type("Applied 2", "fn x => 3 + x ;", tArrow(tInt, tInt)) ;
    do_test_type("List 1", "[];", tList(tV 0)) ;
    do_test_type("Eq type 2", "(fn x => x) = (fn y => y);", tBool) ;

    true
  )


  fun run_tests(arg0 : string, args : string list) = 
    BackTrace.monitor(fn () =>
      (U.run_tests(fn () => (
        test_ast() ; 
        test_eval() ; 
        test_eval_file() ;
        test_typing()
        )) ; 0)
    )

end
