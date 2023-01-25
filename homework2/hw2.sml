(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* Problem 1 *)
(* Test 1 *)
fun all_except_option (item, list) =
   case list of
      [] => NONE
      | x::xs => if same_string(item, x)
                 then SOME xs
                 else all_except_option(item, xs)

(* Test 2 *)
fun get_substitutions1 (list, item) =
   case list of
      [] => []
      | x::xs => case all_except_option (item, x) of
                    NONE => []
                  | SOME y => y @ get_substitutions1 (xs, item)


(* Test 3 *)
fun get_substitutions2 (list, item) =
   let fun get_subs (ls, it, acc)=
      case ls of
           [] => acc
         | head::rest => case all_except_option (it, head) of
                             NONE => acc
                           | SOME y => get_subs (rest, it, y @ acc)
   in
      get_subs (list, item, [])
   end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* Problem 2 *)
