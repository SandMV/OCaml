type distribution =
    { total : int ;
      amounts : (string * int) list }

let compute_distribution str_list = 
  let sorted = List.fast_sort String.compare str_list in
  let rec occurrences sorted_list example acc = 
    match sorted_list with
      | h::t when h = example -> occurrences t example (acc+1)
      | _ -> (example, acc), sorted_list
  and
    compute_distribution acc = function
    | h::t -> 
        let distrib, rest = occurrences t h 1 in
          compute_distribution (distrib :: acc) rest
    | _ -> acc
  in {total = List.length str_list; amounts = compute_distribution [] sorted}

let build_htable words =
  let size = List.length words in
  let intermediate = Hashtbl.create size in
  let result = Hashtbl.create size in
  let rec compute_successors prev_word = function
    | word::rest -> 
        let successors = word::(Hashtbl.find intermediate prev_word) in
          begin
            Hashtbl.add intermediate prev_word successors; 
            if not (Hashtbl.mem intermediate word) then
              Hashtbl.add intermediate word [];
            compute_successors word rest
          end
    | _ ->
        let successors = "STOP"::(Hashtbl.find intermediate prev_word) in 
          Hashtbl.add intermediate prev_word successors
  in
    begin 
      Hashtbl.add intermediate "START" []; 
      compute_successors "START" words;
      Hashtbl.iter (fun key value -> 
                     if not (Hashtbl.mem result key) then
                       Hashtbl.add result key (compute_distribution value)
                   ) intermediate;
      result
    end

let next_in_htable htable word = 
  let distribution = (Hashtbl.find htable word) in
  let nth = Random.int distribution.total in
  let rec get_random_word nth = function
    | (word, amount)::t -> 
        if nth >= amount then
          get_random_word (nth - amount) t
        else
          word
    | _ -> failwith "Unexpected exception"
  in get_random_word nth distribution.amounts

let walk_htable htable = 
  let rec generate_sentence prev_word sentence = 
    let next_word = next_in_htable htable prev_word in
      if String.compare "STOP" next_word <> 0 then
        generate_sentence next_word (next_word :: sentence) 
      else
        List.rev sentence
  in
    generate_sentence "START" []
