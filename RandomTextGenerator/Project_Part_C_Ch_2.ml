type distribution =
    { total : int ;
      amounts : (string * int) list }

type ptable =
    { prefix_length : int ;
      table : (string list, distribution) Hashtbl.t }

let merge_distributions d1 d2 = 
  let rec merge_amounts a1 a2 acc = 
    match a1, a2 with
      | ((word1, am1) as h1 :: rest1), ((word2, am2) as h2 :: rest2) ->
          let comparison = String.compare word1 word2 in
            if comparison = -1 then
              merge_amounts rest1 a2 (h1 :: acc)
            else 
            if comparison = 1 then
              merge_amounts a1 rest2 (h2 :: acc)
            else
              merge_amounts rest1 rest2 ((word1, am1 + am2) :: acc)
      | [], [] -> acc
      | [], _ -> acc @ a2
      | _, [] -> acc @ a1
  in
    {total = d1.total + d2.total; 
     amounts = merge_amounts d1.amounts d2.amounts []}

let check_compatibility ptables = 
  let p_length = (List.hd ptables).prefix_length in
  let rec check = function
    | {prefix_length = p; _} :: rest -> p = p_length && check rest
    | _ -> true
  in check ptables

let merge_htables acc t2 = 
  let add k v = 
    if Hashtbl.mem acc k then
      Hashtbl.add acc k (merge_distributions (Hashtbl.find acc k) v)
    else
      Hashtbl.add acc k v
  in
    Hashtbl.iter add t2; acc

let merge_ptables ptables =
  if check_compatibility ptables then
    let p_length = (List.hd ptables).prefix_length 
    in
    let fold 
          {prefix_length = p_acc; table = t_acc} 
          {prefix_length = p; table = t} =
      {prefix_length = p_acc; table = (merge_htables t_acc t)} 
    and 
      empty = {prefix_length = p_length; table = Hashtbl.create 1}
    in
      List.fold_left fold empty ptables
  else
    failwith "Incompatible prefix_length"

