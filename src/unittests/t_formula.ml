open OUnit2;;
open Formula;;

(** asserts that two formulas are equal *)
let eq_fm expected actual ctxt = assert_equal ~ctxt ~printer:(fun fm -> Format.asprintf "%a" Formula_pp.pp_formula fm) expected actual
let tests = [];;

(** makes a formula from a list of entries *)
let from_list l = List.fold_left (fun fm entry -> push_front entry fm) empty l

(** makes a formula from a smt string
 * NOTE: Does all sorts of simlifications like splitting (assert (and a b)) in
 * (assert a) (assert b) or simplifying constant operations. *)
let from_string s =
  let smt =
    try
      Parse_utils.read_string
      ~parser:Smtlib_parser.script
      ~lexer:Smtlib_lexer.token
      ~string:s
    with Failure(s) as e -> Format.eprintf "Failure: %s" s; raise e
  in
  Smtlib_to_formula.script smt

(* actual tests start here *)

let formula = from_string "(declare-fun useless () Bool)"

let tests = ("prune useless decl">::(eq_fm empty (Formula_transformation.prune_and_inline formula)))::tests;;

let formula = [mk_define(mk_bl_def (bl_var "pa") [] mk_bl_true)
              ;mk_declare(mk_bl_decl (bl_var "x") [])
              ;mk_define(mk_bl_def (bl_var "pa2") [] (mk_bl_and (mk_bl_var (bl_var "pa")) (mk_bl_var (bl_var "x"))))
              ;mk_assert(mk_bl_var(bl_var "pa2"))]
            |> from_list
let expected = [mk_declare(mk_bl_decl (bl_var "x") [])
              ;mk_assert(mk_bl_var(bl_var "x"))]
            |> from_list
let tests = ("inline true def">::(eq_fm expected (Formula_transformation.prune_and_inline formula)))::tests;;

let formula = [mk_define(mk_bl_def (bl_var "pa") [] mk_bl_true)
              ;mk_declare(mk_bl_decl (bl_var "x") [])
              ;mk_define(mk_bl_def (bl_var "pa2") [] (mk_bl_and (mk_bl_var (bl_var "pa")) (mk_bl_var (bl_var "x"))))
              ;mk_define(mk_bl_def (bl_var "pa3") [] (mk_bl_or (mk_bl_var (bl_var "pa2")) (mk_bl_var (bl_var "x"))))
              ;mk_assert(mk_bl_var(bl_var "pa2"))]
            |> from_list
let expected = [mk_declare(mk_bl_decl (bl_var "x") [])
              ;mk_assert(mk_bl_var(bl_var "x"))]
            |> from_list
let tests = ("inline true def and prune irrelevant">::(eq_fm expected (Formula_transformation.prune_and_inline formula)))::tests;;

(* run the tests *)
let () = run_test_tt_main ("formula simplifications">:::tests)


