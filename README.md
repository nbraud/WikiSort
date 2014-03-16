This is a work-in-progress implementation of WikiSort.

Compile with:

	ocamlopt -unsafe wikiSort.ml
	ocamlfind ocamlopt -linkpkg -package benchmark -unsafe wikiSort.cmx bench.ml

Some results, compared with `Array.sort`:

	Latencies for 500 iterations of "merge_sort", "Array.sort":
	merge_sort: 11.67 WALL (10.88 usr +  0.77 sys = 11.66 CPU) @ 42.90/s (n=500)
	Array.sort: 22.19 WALL (22.13 usr +  0.03 sys = 22.16 CPU) @ 22.56/s (n=500)
