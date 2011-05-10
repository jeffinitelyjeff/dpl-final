structure Tests =
struct

  structure A = Ast
  structure Lexers = Lexers(ArithLex)

  structure P = Parse
  open P

  structure U = UnitTest

  val sl = fn s => Lexers.string_lexer s

  fun do_test_ast(name, test, exp) = 
      U.do_test(name, fn () => parse (sl test), exp, A.ast2str)

  fun test_parse () = (
    do_test_ast("Parse 1", "5 ;", A.Number(5)) ;
    do_test_ast("Parse 2", "5+7 ;", A.Plus(A.Number(5), A.Number(7))) ;
    do_test_ast("Parse 3", "5*7 ;", A.Times(A.Number(5), A.Number(7))) ;
    do_test_ast("Parse 4", "5+7*9 ;", A.Plus(A.Number(5), A.Times(A.Number(7),
        A.Number(9)))) ;
    do_test_ast("Parse 5", "5*7+9 ;", A.Plus(A.Times(A.Number(5), A.Number(7)),
        A.Number(9))) ;

    do_test_ast("Paren 1", "(5+7)*9;",
        A.Times(A.Plus(A.Number(5),A.Number(7)), A.Number(9))) ;
    do_test_ast("Paren 2", "(3+5*7)*(2+4);",
        A.Times(A.Plus(A.Number(3),A.Times(A.Number(5),A.Number(7))),
                A.Plus(A.Number(2), A.Number(4)))) ;
    do_test_ast("Paren 3", "((3+5)*7)*(2+4);",
        A.Times(A.Times(A.Plus(A.Number(3),A.Number(5)),A.Number(7)),
                A.Plus(A.Number(2), A.Number(4)))) ;
    do_test_ast("Paren 4", "(3) ;", A.Number(3)) ;

    do_test_ast("Neg 1", "~3 ;", A.Neg(A.Number(3))) ;
    do_test_ast("Neg 2", "~(2+5) ;", A.Neg(A.Plus(A.Number(2), A.Number(5)))) ;
    do_test_ast("Neg 3", "~~5 ;", A.Neg(A.Neg(A.Number(5)))) ;
    do_test_ast("Neg 4", "~(2+5) + 7;",
        A.Plus(A.Neg(A.Plus(A.Number(2),A.Number(5))), A.Number(7))) ;
    do_test_ast("Neg 5", "~2 + 5 ;",
        A.Plus(A.Neg(A.Number(2)), A.Number(5))) ;
    true
  )

  fun run_tests(arg0 : string, args : string list) =
    BackTrace.monitor(fn () => (U.run_tests(fn () => test_parse()) ; 0))

end
