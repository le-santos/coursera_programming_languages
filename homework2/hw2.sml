(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* Problem 1 *)
(* Test 1 *)
fun all_except_option (item, list) =
   let fun check_item (item, list) =
         case list of
              [] => []
            | x::xs => if same_string(item, x) then xs
                       else x::check_item(item, xs)
   in
      case check_item(item, list) of
           [] => SOME []
         | x::xs => if (x::xs) = list then NONE else SOME (x::xs)
   end

(* Test 2 *)
fun get_substitutions1 (list, item) =
   case list of
      [] => []
      | head::rest => case all_except_option (item, head) of
                          NONE => if rest = [] then []
                                  else get_substitutions1 (rest, item)
                        | SOME z => z @ get_substitutions1 (rest, item)

(* Test 3 *)
fun get_substitutions2 (list, item) =
   let fun get_subs (ls, it, acc)=
      case ls of
           [] => acc
         | head::rest => case all_except_option (it, head) of
                             NONE => if rest = [] then acc
                                     else get_subs (rest, it, acc)
                           | SOME y => get_subs (rest, it, acc @ y)
   in
      get_subs (list, item, [])
   end

(* Test 4 *)
fun similar_names (lists_of_names, name_record) =
   let
      val alt_names = get_substitutions1 (lists_of_names, #first name_record)
      fun build_names (first_names, base_name, acc) =
         case first_names of
            [] => base_name::acc
          | x::xs => build_names(xs,
                                 base_name,
                                 acc @ [{ first = x,
                                          last = #last base_name,
                                          middle = #middle base_name}]
                                 )
   in
      build_names(alt_names, name_record, [])
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
