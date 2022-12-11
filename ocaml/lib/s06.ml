module CBuff = struct
  type t = { mutable first : int; mutable length : int; data : bytes }
  (** Fixed capacity circular buffer. *)

  let create cap = { first = 0; length = 0; data = Bytes.create cap }
  let length b = b.length
  let capacity b = Bytes.length b.data

  let pp fmt b =
    Format.fprintf fmt "{(%d) first = %d; length = %d; data = \""
      (Bytes.length b.data) b.first b.length;
    for i = 0 to b.length - 1 do
      let c = Bytes.get b.data ((b.first + i) mod capacity b) in
      let c = if c < 'A' then '?' else c in
      Format.fprintf fmt "%c" c
    done;
    for _ = b.length to capacity b - 1 do
      Format.fprintf fmt "_"
    done;
    Format.fprintf fmt "\" }"

  let take_front b =
    assert (length b > 0);
    let c = Bytes.get b.data b.first in
    b.first <- (b.first + 1) mod capacity b;
    b.length <- b.length - 1;
    c

  let add_back b c =
    let cap = capacity b in
    if length b == cap then ignore (take_front b);
    let idx = (b.first + b.length) mod cap in
    Bytes.set b.data idx c;
    b.length <- b.length + 1

  (** When inserting a character c, if there already is an occurrence of c in the buffer,
      drop all trailing characters as well as the first occurrence, since we know
      the next potential sequence can only start after the dropped ocurrence of c. *)
  let insert_unique b c =
    let cap = capacity b in
    let rec loop len i =
      if i < len then begin
        let d = Bytes.get b.data ((b.first + i) mod cap) in
        if d = c then begin
          b.first <- (b.first + i + 1) mod cap;
          b.length <- b.length - i - 1
        end
        else loop len (i + 1)
      end
    in
    loop b.length 0;
    add_back b c
end

let solve n () =
  let q = CBuff.create n in
  let rec loop i =
    match input_char stdin with
    | c ->
        CBuff.insert_unique q c;
        let len = CBuff.length q in
        if len = n then i else loop (i + 1)
    | exception End_of_file -> i
  in
  Printf.printf "%d\n" (loop 1)

module Sol = struct
  let name = "06"
  let solve_part1 = solve 4
  let solve_part2 = solve 14
end
