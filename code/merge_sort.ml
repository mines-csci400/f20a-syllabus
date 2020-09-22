(*
merge_sort: [100;20;30;14;50;160;70]

step 1: split: [100;20;30]  [14;50;160;70]
step 2: recursively merge_sort each half: [20;30;100]  [14;50;70;160]
step 3: merge the sorted halfs in linear time: [14;20;30;...]
*)

(* test list *)
let x = [100;20;30;14;50;160;70] in

(* print a list *)
let rec print_list = fun x -> (match x with
| [] -> print_string "\n"
| a::more ->
  let _ = print_int a in
  let _ = print_string ", " in
  print_list more) in

(* get length of a list *)
let rec len = fun x -> (match x with
| [] -> 0
| a::more -> 1 + (len more)) in

(*let rec nth = fun (x,n,pos) -> (

) in*)

(* pull from "todo" into "result" for min <= pos < max *)
let rec sub = fun (result,todo,min,max,pos) ->
(match todo with
| [] -> (result,todo,pos)
| a::more ->
  let b = (pos >= min && pos < max) in
  sub ((if b then (result@[a]) else result),more,min,max,pos+1)
) in

(* split a list in half and return (first_half,second_half) *)
let split = (fun x ->
  let half = (len x)/2 in
  let (test1,_,_) = sub ([],x,0,half,0) in
  let (test2,_,_) = sub ([],x,half,len x,0) in
  (test1,test2)
) in

(* merge two sorted lists *)
let rec merge = fun (x,y) -> (match (x,y) with
  | ((a::more1),(b::more2)) ->
    if a < b then (a::(merge (more1,(b::more2))))
    else (b::(merge ((a::more1),more2)))
  | (more1,[]) -> more1
  | ([],more2) -> more2
) in

(*****************************************************)
(** merge sort                                      **)
(*****************************************************)
let rec merge_sort = fun x ->
(match x with
| [] -> []
| [x] -> [x]
| _ ->
  let (one,two) = split x in
  let one = merge_sort one in
  let two = merge_sort two in
  merge (one,two)
) in

let _ = print_string "input list: " in
let _ = print_list x in

(* test the split function *)
let (test1,test2) = split x in
let _ = print_string "split on input list:\n" in
let _ = print_list test1 in
let _ = print_list test2 in

(* test the merge function *)
let _ = print_string "basic merge test:\n" in
let (test1,test2) = ([1;10;100;1000],[20;220;230;2000]) in
let _ = print_list test1 in
let _ = print_list test2 in
let test3 = merge (test1,test2) in
let _ = print_string "merge result: " in
let _ = print_list test3 in

(* test merge_sort *)
let _ = print_string "merge_sort on input list: " in
let test4 = merge_sort x in
let _ = print_list test4 in
()
