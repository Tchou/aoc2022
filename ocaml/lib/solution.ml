let table = Hashtbl.create 16
let register (n : string) (solve : unit -> unit) = Hashtbl.replace table n solve
let get (n : string) = Hashtbl.find table n
let has (n : string) = Hashtbl.mem table n