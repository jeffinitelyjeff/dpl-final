structure UnitTest =
struct

  fun fail_msg(name, exp, act) =
    "FAILURE (" ^ name ^ "): expected " ^ exp ^ "; got " ^ act ^ "."
  fun exn_msg(name, exp) =
    "FAILURE (" ^ name ^ "): expected " ^ exp ^ " to be raised, but wasn't."

  val test_failures = ref([]) : (string list) ref
  fun record_fail(msg) =
    test_failures := msg :: !test_failures
    
  fun do_test(test_name, test, exp, to_str) =
  let
    val act = (print (test_name ^ "...") ; test())
  in
    if exp = act then (print "passed.\n" ; true)
    else (record_fail (fail_msg(test_name, to_str(exp), to_str(act))) ;
                                print "FAILED.\n" ; true)
  end

  fun do_test_eq(test_name, test, exp, eq_test, to_str) =
  let
    val act = (print (test_name ^ "...") ; test())
  in
    if eq_test act exp then (print "passed.\n" ; true)
    else (record_fail (fail_msg(test_name, to_str(exp), to_str(act))) ;
                                print "FAILED.\n" ; true)
  end

  fun assert_bool exp (test_name, test) =
    do_test(test_name, test, exp, Bool.toString)
  val assert_true = assert_bool true
  val assert_false = assert_bool false

  fun assert_int_eq (test_name, test, exp) =
    do_test(test_name, test, exp, Int.toString)

  fun assert_raise (test_name, test, exc) =
    (* This doesn't actually work, so everyone gets exception-throwing
    *  tests for free...
    *)
    (test() ; record_fail(exn_msg(test_name, exnName exc)) ; print "F" ; true)
    handle exc => (print "." ; true)

  fun print_failures [] = ()
    | print_failures (tf::tfs) = (print (tf ^ "\n") ; print_failures tfs)

  fun run_tests tests = (
    tests() ; print "\n" ; print_failures(rev (!test_failures))
  )

end
