
let punctuation = [';'; ','; ':'; '-'; '"'; '\'']
let terminal_punctuation = ['?'; '!'; '.']


let is_word_symbol ch = Char.code ch >= 128 && Char.code ch <= 255 || 
                        is_alphanumeric ch
let is_punctuation ch = List.exists (fun c -> c = ch) punctuation
let is_terminal ch = List.exists (fun c -> c = ch) terminal_punctuation

type word_type = 
    |WORD of string 
    |PUNCTUATION of string 
    |TERMINAL of string 
    |DELIMITER

let get_type ch = 
  if is_word_symbol ch then
    WORD (String.make 1 ch)
  else
  if is_punctuation ch then
    PUNCTUATION (String.make 1 ch)
  else
  if is_terminal ch then
    TERMINAL (String.make 1 ch)
  else
    DELIMITER

let next_word start_pos str = 
  let first_symbol = String.get str start_pos in
  let first_type = get_type first_symbol in
    match first_type with
      | DELIMITER -> (DELIMITER, start_pos + 1)
      | PUNCTUATION ch | TERMINAL ch as t -> 
          (t, start_pos + 1)
      | WORD _ -> 
          let rec get_word pos acc =
            if pos < String.length str then
              if is_word_symbol (String.get str pos) then
                get_word (pos+1) (acc ^ String.make 1 (String.get str pos))
              else
                (acc, pos)
            else
              (acc, pos)
          in
          let (word, pos) = get_word start_pos "" in
            (WORD word, pos)

let next_sentence start_pos str =
  let rec get_sentence pos acc =
    if pos < String.length str then
      let (word, next_pos) = next_word pos str in
        match word with 
          | DELIMITER -> get_sentence next_pos acc 
          | PUNCTUATION w | WORD w -> get_sentence next_pos (w :: acc)
          | TERMINAL t -> (List.rev (t :: acc), next_pos)
    else (List.rev acc, pos)
  in get_sentence start_pos []

let sentences str = 
  let rec sentences pos acc = 
    if pos < String.length str then
      let (sentence, next_pos) = next_sentence pos str in
        if List.length sentence > 0 then
          sentences next_pos (sentence :: acc)
        else
          List.rev acc
    else
      List.rev acc
  in sentences 0 []
;;


let start length = 
  let rec mk_list length acc = 
    if length > 0 then
      mk_list (length - 1) ("START" :: acc)
    else
      acc
  in mk_list length []

let shift l e = 
  match l with
    | h::t -> t @ [e]
    | _ -> []

let build_ptable str_list prefix_length = 
  let prefix = start prefix_length in 
  let htable = Hashtbl.create (List.length str_list) in
  let rec build_htable prev_prefix = function 
    | word::rest -> 
        let new_value = word :: (Hashtbl.find htable prev_prefix) in
        let next_prefix = shift prev_prefix word in
          begin
            Hashtbl.add htable prev_prefix new_value;
            if not (Hashtbl.mem htable next_prefix) then
              Hashtbl.add htable next_prefix []; 
            build_htable next_prefix rest
          end
    | _ -> 
        let new_value = "STOP" :: (Hashtbl.find htable prev_prefix) in
          Hashtbl.add htable prev_prefix new_value
  in 
    begin
      Hashtbl.add htable prefix [];
      build_htable prefix str_list;
      let distrib_table = Hashtbl.create (List.length str_list) in
        Hashtbl.iter (fun key value ->
                       if not (Hashtbl.mem distrib_table key) then 
                         Hashtbl.add distrib_table key (compute_distribution value)
                     ) htable;
        {prefix_length = prefix_length; table = distrib_table}
    end 

let walk_ptable {table; prefix_length = length} = 
  let rec generate_sentence prev_prefix sentence = 
    let next_word = next_in_htable table prev_prefix in
      if String.compare "STOP" next_word <> 0 then
        generate_sentence (shift prev_prefix next_word) (next_word :: sentence) 
      else
        List.rev sentence
  in
    generate_sentence (start length) []


let merge_ptables tl =
  "Replace this string with your implementation." ;;
