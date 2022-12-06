module CBuff = struct
  type t = { mutable first : int; mutable length : int; data : bytes }
  (** Fixed capacity circular buffer. *)

  let create cap = { first = 0; length = 0; data = Bytes.create cap }
  let length b = b.length
  let capacity b = Bytes.length b.data

  let add_back b c =
    let cap = capacity b in
    assert (length b < cap);
    let idx = (b.first + b.length) mod cap in
    Bytes.set b.data idx c;
    b.length <- b.length + 1

  let take_front b =
    assert (length b > 0);
    let c = Bytes.get b.data b.first in
    b.first <- (b.first + 1) mod capacity b;
    c

  (** When inserting a character c, if there already is an occurrence of c in the buffer,
      drop all trailing characters as well as the first occurrence, since we know
      then next potential sequence can only start after the dropped ocurrence of c. *)
  let insert_unique b c =
    let cap = capacity b in
    let rec loop len i =
      if i < len then begin
        let d = Bytes.get b.data ((b.first + i) mod cap) in
        if d = c then begin
          b.first <- b.first + i + 1;
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
        if len = n then i
        else
          let () = if len > n then ignore (CBuff.take_front q) in
          loop (i + 1)
    | exception End_of_file -> assert false
  in
  Printf.printf "%d\n" (loop 1)

let name = "06a"
let () = Solution.register name (solve 4)
let name = "06b"
let () = Solution.register name (solve 14)
