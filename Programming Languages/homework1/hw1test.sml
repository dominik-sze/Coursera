(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


val test1_1 = is_older((2013,2,10),(2014, 5,2)) = true
val test1_2 = is_older((2014,2,10),(2013, 5,2)) = false
val test1_3 = is_older((2013,2,10),(2013, 5,2)) = true
val test1_4 = is_older((2013,5,10),(2013, 2,2)) = false
val test1_5 = is_older((2013,5,10),(2013,5,12)) = true
val test1_6 = is_older((2013,5,10),(2013, 5,2)) = false
val test1_7 = is_older((2013,5,10),(2013,5,10)) = false

val test2_1 = number_in_month([],2) = 0
val test2_2 = number_in_month([(2012,2,28),(2013,12,1),(2014,10,9),(2010,2,8)],2) = 2
val test2_3 = number_in_month([(2012,2,28),(2013, 2,1),(2014, 2,9),(2010,2,8)],2) = 4
val test2_4 = number_in_month([(2012,2,28),(2013,12,1),(2014,10,9),(2010,2,8)],3) = 0

val test3_1 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0
val test3_2 = number_in_months([],[2,3,4]) = 0
val test3_3 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3_4 = number_in_months([(2012,2,28),(2013,2,1),(2011,3,31),(2011,3,28)],[2,5,3,4]) = 4

val test4_1 = dates_in_month([],2) = []
val test4_2 = dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4_3 = dates_in_month([(2012,2,28),(2013,12,1)],3) = []
val test4_4 = dates_in_month([(2012,2,28),(2013,12,1),(2013,2,1),(2010,2,16)],2) = [(2012,2,28),(2013,2,1),(2010,2,16)]

val test5_1 = dates_in_months([],[2,3,4]) = []
val test5_2 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = []
val test5_3 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test5_4 = dates_in_months([(2012,2,28),(2013,2,1),(2011,3,31),(2011,4,28)],[2,4]) = [(2012,2,28),(2013,2,1),(2011,4,28)]

val test6_1 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"
val test6_2 = get_nth(["hi", "there", "how", "are", "you"], 1) = "hi"
val test6_3 = get_nth(["hi", "there", "how", "are", "you"], 5) = "you"

val test7_1 = date_to_string((2013, 6, 1)) = "June 1, 2013"
val test7_2 = date_to_string((2010, 1, 30)) = "January 30, 2010"

val test8_1 = number_before_reaching_sum(10, [1,2,3,4,5]) = 3
val test8_2 = number_before_reaching_sum(1, [1,2,3,4,5]) = 0
val test8_3 = number_before_reaching_sum(2, [1,2,3,4,5]) = 1
val test8_4 = number_before_reaching_sum(3, [1,2,3,4,5]) = 1
val test8_5 = number_before_reaching_sum(4, [1,2,3,4,5]) = 2

val test9_1 = what_month(2) = 1
val test9_2 = what_month(31) = 1
val test9_3 = what_month(32) = 2
val test9_4 = what_month(70) = 3
val test9_5 = what_month(333) = 11
val test9_6 = what_month(363) = 12

val test10_1 = month_range(34, 31) = []
val test10_2 = month_range(31, 34) = [1,2,2,2]
val test10_3 = month_range(1, 1) = [1]
val test10_4 = month_range(364, 365) = [12, 12]

val test11_1 = oldest([]) = NONE
val test11_2 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val test12_1 = remove_duplicates([1,1,2,1,4,3,1,2,4],[]) = [1,2,4,3]

val test13_1 = number_in_months_challenge([],[2,3,2,3,4,2,4]) = 0
val test13_2 = number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,2,3,4,2]) = 3
val test13_3 = number_in_months_challenge([(2012,2,28),(2013,2,1),(2011,3,31),(2011,3,28)],[2,5,3,4,2,2]) = 4

val test14_1 = dates_in_months_challenge([],[2,3,2,4,3,2,4]) = []
val test14_2 = dates_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,2,3,4,2,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test14_3 = dates_in_months_challenge([(2012,2,28),(2013,2,1),(2011,3,31),(2011,4,28)],[2,4,2,4,2,2]) = [(2012,2,28),(2013,2,1),(2011,4,28)]

val test15_1 = reasonable_date((2012,2,28)) = true
val test15_2 = reasonable_date((2012,2,29)) = true
val test15_3 = reasonable_date((2013,2,29)) = false
val test15_4 = reasonable_date((2013,1,31)) = true
val test15_5 = reasonable_date((2013,4,31)) = false
val test15_6 = reasonable_date((2013,~1,20)) = false
val test15_7 = reasonable_date((2013,1,~20)) = false
val test15_8 = reasonable_date((~2013,4,31)) = false
val test15_9 = reasonable_date((2013,14,20)) = false
val test15_10 = reasonable_date((2013,14,20)) = false
val test15_11 = reasonable_date((0,2,28)) = false

