open WikiSort

let () = Random.init 424242

let shuffle t =
  for i=1 to Array.length t -1 do
	swap t i @@ Random.int (i+1)
  done

let _ =
  let n = 100_000 in
  let l = Array.make n (fun i -> i) in
  shuffle l;
  Benchmark.latencyN 500L
	["merge_sort", merge_sort Pervasives.compare, Array.copy l;
	 "Array.sort", Array.sort Pervasives.compare, Array.copy l]

(* Running with -unsafe (un-guarded array access)
   Latencies for 500 iterations of "merge_sort", "Array.sort":
merge_sort:  1.43 WALL ( 1.42 usr +  0.00 sys =  1.43 CPU) @ 350.45/s (n=500)
Array.sort: 23.00 WALL (22.94 usr +  0.04 sys = 22.98 CPU) @ 21.76/s (n=500
*)
