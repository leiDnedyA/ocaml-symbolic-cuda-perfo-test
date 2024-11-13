module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)

let count_accesses (enabled_array: bool array) (warp_count: int) (word_size: int): int = 
  let accesses = ref IntSet.empty in
  for i = 1 to warp_count do
    if enabled_array.(i - 1) == true then 
      accesses := IntSet.add ((i - 1) / word_size) !accesses;
  done;
  IntSet.cardinal !accesses

let enabled = [|true; false; true; true; true|]
let warp_count = Array.length enabled
let word = 4

Printf.printf "%d\n" (count_accesses enabled warp_count word) 
