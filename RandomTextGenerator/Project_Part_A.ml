let alphanumeric = 
  ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 
   'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z';
   'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o';
   'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z';
   '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']
let is_alphanumeric ch = 
  let rec check = function
    | h::t -> h = ch || check t
    | [] -> false
  in check alphanumeric

let next_word str start_pos end_pos = 
  let rec accumulate_next i acc = 
    if i < end_pos && is_alphanumeric (String.get str i) then
      accumulate_next (i+1) (acc ^ Char.escaped (String.get str i))
    else 
      (i, acc)
  in accumulate_next start_pos ""

let words str =
  let str_len = String.length str in
  let rec get_words start_pos =
    if start_pos < str_len then
      let (next_start, word) = next_word str start_pos str_len in
        if String.compare word "" <> 0 then
          word :: get_words (next_start + 1)
        else
          get_words (next_start + 1)
    else
      []
  in get_words 0 
;;

let build_ltable str_list = 
  let rec build_ltable prev_word acc = function
    | word::tl -> 
        let new_val = word :: List.assoc prev_word acc in
        let new_acc = (prev_word, new_val) :: (List.remove_assoc prev_word acc) in
          if List.mem_assoc word new_acc then
            build_ltable word new_acc tl
          else
            build_ltable word ((word, []) :: new_acc) tl
    | [] -> 
        let new_val = "STOP" :: List.assoc prev_word acc in
          (prev_word, new_val) :: (List.remove_assoc prev_word acc)
  in build_ltable "START" [("START", [])] str_list

let next_in_ltable ltable word = 
  let list_of_followers = List.assoc word ltable in
  let nth = Random.int (List.length list_of_followers) in
    List.nth list_of_followers nth

let walk_ltable ltable = 
  let rec generate_sentence prev_word sentence = 
    let next_word = next_in_ltable ltable prev_word in
      if String.compare next_word "STOP" <> 0 then 
        generate_sentence next_word (next_word :: sentence)
      else
        List.rev sentence
  in generate_sentence "START" []
