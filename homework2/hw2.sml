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
            | x::xs => if same_string(item, x) then xs else x::check_item(item, xs)
   in
      if [item] = list then SOME []
      else case check_item(item, list) of
            [] => NONE
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

fun card_color (card) =
   case card of
      (Clubs,rank)  => Black
    | (Spades,rank) => Black
    | (others,rank) => Red

fun card_value (card) =
   case card of
      (suit, Num x) => x
    | (suit, Ace) => 11
    | (suit, others) => 10

fun remove_card (card_list, card, ex) =
   case card_list of
      []    => raise ex
      | x::xs => if x = card then xs else x::remove_card(xs, card, ex)

fun all_same_color (card_list) =
   case card_list of
        [] => true
      | [x] => true
      | head::(neck::rest) => card_color(head) = card_color(neck) andalso all_same_color(neck::rest)

fun sum_cards (card_list) =
   let fun add_values(cards, acc) =
            case cards of
                [] => acc
              | first::rest => add_values(rest, acc + card_value(first))
   in
     add_values(card_list, 0)
   end

fun score (held_cards, goal) =
   let fun pre_score (cards, goal) = 
      let val sum = sum_cards(cards)
      in
         if sum > goal then 3 * (sum - goal)
         else (goal - sum)
      end
   in
     if all_same_color(held_cards) then pre_score(held_cards, goal) div 2
     else pre_score(held_cards, goal)
   end

fun officiate (card_list, move_list, goal) =
   let
      fun draw_cards (game_cards, held_cards) =
         case game_cards of
            [] => held_cards
          | x::xs => x::held_cards

      fun change_held_cards (game_cards, held_cards, move) =
         case move of
            Discard card => remove_card (held_cards, card, IllegalMove) (* remove specific card from held_cards *)
          | Draw => draw_cards (game_cards, held_cards) (* remove 1st from game_cards and add to held cards *)
      
      fun change_game_cards (cards) =
         case cards of
            [] => []
          | x::xs => xs

      fun process_moves (game_cards, held_cards, moves, goal) =
         if sum_cards (held_cards) > goal then held_cards
         else case moves of
                  [] => held_cards
                | first_move::next_moves => 
                        case first_move of
                           Discard card => process_moves(game_cards, change_held_cards(game_cards, held_cards, first_move), next_moves, goal)
                         | Draw => process_moves(change_game_cards(game_cards), change_held_cards(game_cards, held_cards, first_move), next_moves, goal)
   in
     score(process_moves(card_list, [], move_list, goal), goal)
   end

