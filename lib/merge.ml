let uniq xs =
  let rec aux xs =
    match xs with
    | x1::(x2::_ as tl) -> if x1 = x2 then aux xs else x1::aux tl
    | [_] | [] -> xs
  in
  aux (List.sort compare xs)

let id x = x

let assoc_pop k l2 =
  let rec aux k l2 acc =
    match l2 with
    | ((a,b) as hd)::tl -> if a = k then Some (b, tl @ acc) else aux k tl (hd::acc)
    | [] -> None
  in
  aux k l2 []

let rec merge_assoc ~strat l1 l2 =
  match l1 with
  | ((k,v1) as hd) ::tl -> begin
      match assoc_pop k l2 with
      | Some (v2,rest) -> (k,merge v1 v2)::merge_assoc ~strat tl rest
      | None -> hd::merge_assoc ~strat tl l2
    end
  | [] -> l2

and merge
    ?(o_strat=`Key_merge)
    ?(a_strat=`Replace)
    (base: Yojson.Safe.t)
    (ext: Yojson.Safe.t) =
  match base, ext with
  | `Assoc base_l, `Assoc ext_l -> `Assoc (merge_assoc ~strat:o_strat base_l ext_l)
  | `List l1 , `List l2 ->  begin
    match a_strat with
    | `Replace -> `List l2
    | `Keep -> `List l1
    | `Append unique -> `List ((if unique then uniq else id) (l1 @ l2))
    | `Other f -> f l1 l2
  end
  | _, b -> b
