open Utils.Syntax.Hashtbl

module Chamber = struct
  type t = {
    mutable count : int;
    mutable tunnel : Bytes.t;
    mutable top : int; (*top-most occupied row*)
  }

  let () = Printexc.record_backtrace true

  type rock = int * int * Bytes.t

  let make () =
    let b = Bytes.make 1024 '\x00' in
    { count = 0; top = -1; tunnel = b }

  let resize c =
    let len = Bytes.length c.tunnel in
    let nlen = 3 * len / 2 in
    let ntunnel = Bytes.make nlen '\x00' in
    Bytes.blit c.tunnel 0 ntunnel 0 len;
    c.tunnel <- ntunnel

  let ensure c i j =
    let idx = (((j * 8) + i) lsr 3) + 8 in
    if idx >= Bytes.length c.tunnel then resize c

  let mk_rock (i, j, t) =
    let len = Array.length t in
    let b = Bytes.make len '\x00' in
    for i = 0 to len - 1 do
      Bytes.set b i (Char.chr t.(i))
    done;
    i, j, b

  let hbar_rock = mk_rock (4, 1, [| 0b1111 |])
  let cross_rock = mk_rock (3, 3, [| 0b10111010; 0 |])
  let l_rock = mk_rock (3, 3, [| 0b00100111; 1 |])
  let vbar_rock = mk_rock (1, 4, [| 0b1111 |])
  let square_rock = mk_rock (2, 2, [| 0b1111 |])
  let rocks = [| hbar_rock; cross_rock; l_rock; vbar_rock; square_rock |]

  let not_0000 b =
    let rec loop i =
      if i >= 5 then true else (b lsr i) land 0b1111 != 0 && loop (i + 1)
    in
    loop 0

  let invalid_for_hbar = Array.init 256 not_0000

  let get_bit buff w i j =
    assert (i < w);
    let num_bit = (j * w) + i in
    let b = Bytes.get_uint8 buff (num_bit lsr 3) in
    let o = num_bit land 7 in
    (b lsr o) land 1

  let set_bit buff w i j v =
    let num_bit = (j * w) + i in
    let idx = num_bit lsr 3 in
    let b = Bytes.get_uint8 buff idx in
    let o = num_bit land 7 in
    let mask = 1 lsl o in
    let mask = if v == 1 then b lor mask else lnot b land mask in
    if v == 1 then Bytes.set_uint8 buff idx mask

  let test_rock c r x y =
    (* tries to put rock r with its lowest left corner in x y
       in the tunnel c.
       *)
    let tunnel = c.tunnel in
    let wr, hr, buffr = r in
    ensure c 7 (y + hr + 1);
    try
      for j = 0 to hr - 1 do
        let yj = y + j in
        for i = 0 to wr - 1 do
          let xi = x + i in
          if
            xi < 0 || yj < 0 || xi > 6
            || get_bit buffr wr i j land get_bit tunnel 8 xi yj == 1
          then raise_notrace Exit
        done
      done;
      true
    with Exit -> false

  let put_rock c r x y =
    let tunnel = c.tunnel in
    let wr, hr, buffr = r in
    ensure c 7 (y + hr + 1);
    for j = 0 to hr - 1 do
      for i = 0 to wr - 1 do
        let xi = x + i in
        set_bit tunnel 8 xi (y + j) (get_bit buffr wr i j)
      done
    done

  let render fmt i c rock xr yr g g_idx r_idx =
    Format.fprintf fmt "%s%!" Utils.clear_screen;
    let height = 25 in
    let top = max height (c.top + 4) in
    let bottom = top - height in
    let wr, hr, buffr = rock in
    for y = top downto bottom do
      let j = y - yr in
      Format.fprintf fmt "% 4d|" y;
      for x = 0 to 6 do
        let i = x - xr in
        let rock_pixel =
          if j >= 0 && j < hr && i >= 0 && i < wr then begin
            let b = get_bit buffr wr i j in
            if b == 1 then begin
              Format.fprintf fmt "@"; true
            end
            else false
          end
          else false
        in
        let b = get_bit c.tunnel 8 x y in
        if not rock_pixel then
          if b == 1 then Format.fprintf fmt "#" else Format.fprintf fmt "."
      done;
      Format.fprintf fmt "|@\n"
    done;
    if bottom = 0 then Format.fprintf fmt "    +-------+@\n%!"
    else Format.fprintf fmt "@\n%!";
    Format.fprintf fmt
      "@\nROUND: %d, GAS: %c, GIDX:%d, RIDX:%d, x=%d, y=%d@\n%!" i g g_idx r_idx
      xr yr;
    Unix.sleepf 0.125

  let fall ?(animate = false) i c rock gas gas_idx =
    let _, hr, _ = rock in
    let gas_len = String.length gas in
    let rec loop x y idx =
      let x1 = x + (Char.code gas.[idx] - 61) in
      if animate then
        render Format.err_formatter i c rock x y gas.[idx] idx
          ((i - 1) mod Array.length rocks);
      let idx = (idx + 1) mod gas_len in
      let x = if test_rock c rock x1 y then x1 else x in
      if animate then
        render Format.err_formatter i c rock x y gas.[idx] idx
          ((i - 1) mod Array.length rocks);
      let y1 = y - 1 in
      if test_rock c rock x y1 then loop x y1 idx else x, y, idx
    in
    let ((_, y, _) as res) = loop 2 (c.top + 4) gas_idx in
    c.top <- max c.top (y + hr - 1);
    res

  let get_top_byte c =
    let idx = (7 + (8 * c.top)) lsr 3 in
    Bytes.get_uint8 c.tunnel idx

  let simulate ?(animate = false) ?(trigger = fun _ _ _ _ -> ()) ?(gas_idx = 0)
      c gas n
    =
    let rock_len = Array.length rocks in
    let rec loop i gas_idx rock_idx =
      if i <= n then begin
        let rock = rocks.(rock_idx) in
        let rock_idx = (rock_idx + 1) mod rock_len in
        let x, y, gas_idx = fall ~animate i c rock gas gas_idx in
        put_rock c rock x y;
        trigger c i rock_idx gas_idx;
        loop (i + 1) gas_idx rock_idx
      end
    in
    loop 1 gas_idx 0; c.top + 1

  let find_period_before c gas n =
    let cache = ~%[] in
    let exception Found of (int * int * int * int * int) in
    let trigger c i rock_idx gas_idx =
      let b = get_top_byte c in
      if rock_idx == 0 && invalid_for_hbar.(b) then
        if cache.%?[gas_idx] then
          let i0, height0 = cache.%[gas_idx] in
          raise (Found (i0, height0, i, c.top + 1, gas_idx))
        else cache.%[gas_idx] <- i, c.top + 1
    in
    try
      ignore (simulate ~trigger c gas n);
      raise Not_found
    with Found x -> x

  let simulate_large c gas n =
    let i0, h0, i1, h1, gas_idx = find_period_before c gas n in
    let period = i1 - i0 in
    let m = (n - i0) / period in
    let rem = (n - i0) mod period in
    h0 + (m * (h1 - h0)) + simulate ~gas_idx (make ()) gas rem
end

let load_level () =
  let gbuff = Buffer.create 16 in
  Utils.fold_chars
    (fun () c -> if c = '<' || c = '>' then Buffer.add_char gbuff c)
    ();
  let gas = Buffer.contents gbuff in
  gas

module Sol = struct
  let name = "17"

  let solve_part1 () =
    let gas = load_level () in
    let chamber = Chamber.make () in
    let n = Chamber.simulate chamber gas 2022 in
    Format.printf "%d@\n" n

  let solve_part2 () =
    let gas = load_level () in
    let chamber = Chamber.make () in
    let n = Chamber.simulate_large chamber gas 1000000000000 in
    Format.printf "%d@\n" n
end

let () = Solution.register_mod (module Sol)

let () =
  Solution.register "17_part1_animate" (fun () ->
      let gas = load_level () in
      let chamber = Chamber.make () in
      let n = Chamber.simulate ~animate:true chamber gas 2022 in
      Format.printf "%d@\n" n)
