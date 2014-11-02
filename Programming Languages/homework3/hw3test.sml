(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1_0 = only_capitals [] = []
val test1_1 = only_capitals ["Aa", "aA", "BB", "bbb", "C"] = ["Aa", "BB", "C"]
val test1_2 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2_0 = longest_string1 [] = ""
val test2_1 = longest_string1 ["A"] = "A"
val test2_2 = longest_string1 ["A", "B"] = "A"
val test2_3 = longest_string1 ["A", "b", "cd"] = "cd"
val test2_4 = longest_string1 ["A", "cd", "b", "aa"] = "cd"
val test2_5 = longest_string1 ["A", "lala", "b", "aa"] = "lala"

val test3_0 = longest_string2 [] = ""
val test3_1 = longest_string2 ["A"] = "A"
val test3_2 = longest_string2 ["A", "B"] = "B"
val test3_3 = longest_string2 ["A", "b", "cd"] = "cd"
val test3_4 = longest_string2 ["A", "cd", "b", "aa"] = "aa"
val test3_5 = longest_string2 ["A", "lala", "b", "aa"] = "lala"

val test4_0 = longest_string3 [] = ""
val test4_1 = longest_string3 ["A"] = "A"
val test4_2 = longest_string3 ["A", "B"] = "A"
val test4_3 = longest_string3 ["A", "b", "cd"] = "cd"
val test4_4 = longest_string3 ["A", "cd", "b", "aa"] = "cd"
val test4_5 = longest_string3 ["A", "lala", "b", "aa"] = "lala"

val test5_0 = longest_string4 [] = ""
val test5_1 = longest_string4 ["A"] = "A"
val test5_2 = longest_string4 ["A", "B"] = "B"
val test5_3 = longest_string4 ["A", "b", "cd"] = "cd"
val test5_4 = longest_string4 ["A", "cd", "b", "aa"] = "aa"
val test5_5 = longest_string4 ["A", "lala", "b", "aa"] = "lala"

val test6_0 = longest_capitalized [] = "";
val test6_1 = longest_capitalized ["A"] = "A";
val test6_2 = longest_capitalized ["a"] = "";
val test6_3 = longest_capitalized ["A","bc","C"] = "A";
val test6_4 = longest_capitalized ["AA","bc","C","cAAC","BaaB"] = "BaaB";

val test7_0 = rev_string "" = "";
val test7_1 = rev_string "abc" = "cba";
val test7_2 = rev_string "aaaa" = "aaaa";
val test7_3 = rev_string "Aaaa" = "aaaA";

val test8_0 = (first_answer (fn x => if x > 3 then SOME x else NONE) []; false) handle NoAnswer => true 
val test8_1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test8_2 = (first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,1,2]; false) handle NoAnswer => true
val test8_3 = first_answer (fn x => if Char.isUpper(String.sub(x, 0)) then SOME x else NONE) ["aA", "abc", "Abc", "Abc", "a"] = "Abc"

val test9_0 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test9_1 = all_answers (fn x => if x < 3 then SOME [x] else NONE) [1,2,0] = SOME [1,2,0]
val test9_2 = all_answers (fn x => SOME [x, x+100]) [1,0,2,0,3,4] = SOME [1,101,0,100,2,102,0,100,3,103,4,104]
val test9_3 = all_answers (fn x => if x < 6 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test10a_0 = count_wildcards Wildcard = 1
val test10a_1 = count_wildcards (Variable("a")) = 0
val test10a_2 = count_wildcards (TupleP[Wildcard,TupleP[Wildcard,Wildcard]]) = 3
val test10a_3 = count_wildcards (TupleP[Wildcard,Variable("a"),ConstP(10),TupleP[Wildcard,UnitP,Wildcard]]) = 3
val test10a_4 = count_wildcards (ConstructorP("a",TupleP[Wildcard,ConstructorP("b",TupleP[ConstP(10),Wildcard])])) = 2
val test10a_5 = count_wildcards (UnitP) = 0

val test10b_0 = count_wild_and_variable_lengths (ConstP(25)) = 0
val test10b_1 = count_wild_and_variable_lengths (Variable("a")) = 1
val test10b_2 = count_wild_and_variable_lengths (Variable("abc")) = 3
val test10b_3 = count_wild_and_variable_lengths (Wildcard) = 1
val test10b_4 = count_wild_and_variable_lengths (ConstructorP("abc",TupleP[Wildcard,Variable("foo"),ConstructorP("b",TupleP[ConstP(10),Wildcard])])) = 5

val test10c_0 = count_some_var ("x", Wildcard) = 0
val test10c_1 = count_some_var ("x", Variable("X")) = 0
val test10c_2 = count_some_var ("x", ConstructorP("x",TupleP[Variable("x"),UnitP,Variable("xx"),TupleP[Wildcard,Variable("x")]])) = 2

val test11_0 = check_pat (Variable("A")) = true
val test11_1 = check_pat (ConstructorP("a",Variable("A"))) = true
val test11_2 = check_pat (TupleP[Variable("a"),Variable("b"),ConstructorP("x",Variable("c")),TupleP[Variable("d"),ConstP(10),Wildcard,TupleP[Variable("e"),TupleP[Variable("x")]]]]) = true
val test11_3 = check_pat (TupleP[Variable("a"),Variable("b"),ConstructorP("x",Variable("c")),TupleP[Variable("d"),ConstP(10),Wildcard,TupleP[Variable("e"),TupleP[Variable("a")]]]]) = false
val test11_4 = check_pat (TupleP[Wildcard, UnitP]) = true
val test11_5 = check_pat (TupleP[Variable("z"), Variable("zz")]) = true

val test12_0 = match (Const(1), UnitP) = NONE
val test12_1 = match (Const(1), Variable("x")) = SOME [("x", Const(1))]
val test12_2 = match (Unit, UnitP) = SOME []
val test12_3 = match (Unit, Wildcard) = SOME []
val test12_4 = match (Unit, ConstructorP("A", UnitP)) = NONE
val test12_5 = match (Constructor("B", Unit), ConstructorP("A", UnitP)) = NONE
val test12_6 = match (Constructor("A", Unit), ConstructorP("A", UnitP)) = SOME []
val test12_7 = match (Constructor("A", Unit), Wildcard) = SOME []
val test12_8 = match (Constructor("A", Const(2)), ConstructorP("A", ConstP(2))) = SOME []
val test12_9 = match (Const(2), ConstP(3)) = NONE
val test12_10 = match (Tuple[], TupleP[]) = SOME []
val test12_11 = match (Tuple[Const(2)], TupleP[Variable("x")]) = SOME [("x", Const(2))]
val test12_12 = match (Tuple[Const(2),Constructor("a",Const(3)),Tuple[Const(4)]],TupleP[Variable("x"),ConstructorP("A",Variable("x")),TupleP[Variable("z")]]) = NONE
val test12_13 = match (Tuple[Const(2),Constructor("a",Const(3)),Tuple[Const(4)]],TupleP[Variable("x"),ConstructorP("a",Variable("y")),TupleP[Variable("z")]]) = 
  SOME [("x", Const(2)),("y", Const(3)),("z", Const(4))]

val test13_0 = first_match Unit [UnitP] = SOME []
val test13_1 = first_match Unit [TupleP[UnitP],UnitP] = SOME []
val test13_2 = first_match Unit [Variable("x")] = SOME [(("x"), Unit)]
val test13_3 = first_match Unit [Wildcard, Variable("x")] = SOME []
val test13_4 = first_match (Constructor("a", Const(1))) [ConstructorP("a", Variable("x"))] = SOME [("x", Const(1))]
val test13_5 = first_match (Constructor("a", Const(1))) [Variable("x"), TupleP[ConstructorP("a", ConstP(1))]] = SOME [("x", Constructor("a", Const(1)))]
val test13_6 = first_match (Tuple[Const(1), Unit]) [TupleP[ConstP(1), UnitP]] = SOME []
val test13_7 = first_match (Tuple[Const(1), Unit]) [TupleP[ConstP(2), UnitP]] = NONE
