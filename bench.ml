open WikiSort;;

let n = 100_000 in
let l = Array.init n (fun i -> n-i) in
Benchmark.latencyN 500L ["merge_sort", merge_sort Pervasives.compare, l;
						 "Array.sort", Array.sort Pervasives.compare, l]

(* Running with -unsafe (un-guarded array access)
      Latencies for 100 iterations of "merge_sort", "Array.sort":
merge_sort: 12.53 WALL (12.49 usr +  0.03 sys = 12.51 CPU) @  7.99/s (n=100)
Array.sort: 39.02 WALL (38.91 usr +  0.06 sys = 38.96 CPU) @  2.57/s (n=100)
*)
