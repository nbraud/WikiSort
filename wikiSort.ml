let is_safe ~len ?(size = 1) i =
  assert((i >= 0) && (i + size <= len) && (i + size >= 0))

let swap t =
  let len = Array.length t in
  fun a b ->
	assert(a < len);
	assert(b < len);
	let tmp = t.(a) in
	t.(a) <- t.(b);
	t.(b) <- tmp

let swap_range t =
  let len = Array.length t
  and swap = swap t in
  fun s_a s_b size ->
	is_safe ~len ~size s_a;
	is_safe ~len ~size s_b;
	assert( (s_a + size) <= s_b || (s_b + size) <= s_a ); (* No aliasing *)
	
	for i=0 to size-1 do
	  swap (s_a+i) (s_b+i)
	done

let rev t =
  let swap = swap t in
  fun start len ->
	for i = 0 to len/2 do
	  swap (start+i) (start+len-i-1)
	done

(* let t = Array.init 10 (fun x -> x) in rev t 0 10; t *)

let rotate t = (* TODO: Optimize for big rotations *)
  let rev = rev t in
  fun a len shift ->
	let shift = if shift < 0 then shift+len else shift in
	rev a shift;
	rev (a+shift) (len-shift);
	rev a len

(* let t = Array.init 10 (fun x -> x) in rotate t 0 10 (-3); t *)

let locate t =
  let len = Array.length t in
  fun x ->
	let rec loop = function
	  | (-1) -> raise Not_found
	  | i -> if t.(i) = x then i else loop (i-1)
	in loop len

let merge ~cmp t s_a s_b s_c =
  let a_len = s_b-s_a and b_len = s_c-s_b+1 in
  let a = Array.sub t s_a a_len in
  let rec aux a' b' i =
	if cmp a.(a') t.(s_b + b') <= 0 then begin
	  t.(i) <- a.(a');
	  if a'+1 = a_len then
		assert(i = s_b + b' -1)
	  else aux (a'+1) b' (i+1)
	end else begin
	  t.(i)<-t.(s_b + b');
	  if b'+1 = b_len then
		for j=1 to a_len-a' do
		  t.(i+j)<-a.(a'+j-1)
		done
	  else aux a' (b'+1) (i+1)
	end in

  if cmp t.(s_a) t.(s_b) <= 0
  then () (* in order *)
  else if (cmp t.(s_a) t.(s_c) > 0) && (a_len = b_len)
  (* TODO: Handle other cases *)
  then (* [A][B] in reverse order -> [B][A] *)
	swap_range t s_a s_b a_len
  else aux 0 0 s_a

let insertion_sort ~cmp t a len =
  for i=a+1 to a+len-1 do
	let tmp = t.(i) in
	begin try
	  for j=i downto a+1 do
		if cmp t.(j-1) tmp <= 0 then begin
		  t.(j)<-tmp;
		  raise Exit
		end;
		t.(j)<-t.(j-1);
	  done;
	  t.(a)<- tmp
	with Exit -> () end
  done

let rec aux_sort ~cmp t a len =
  if len < 32 then
	insertion_sort ~cmp t a len
  else begin
	aux_sort ~cmp t a (len/2);
	aux_sort ~cmp t (a+len/2) (len-len/2);

	merge ~cmp t a (a+len/2) (a+len-1)
  end

let merge_sort cmp t =
  aux_sort ~cmp t 0 @@ Array.length t

(* let t = Array.init 10 (fun x -> x) in rev t 0 10; merge_sort t 0 10; t *)
