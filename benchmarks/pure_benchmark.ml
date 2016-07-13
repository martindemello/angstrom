module R = Result
open Core.Std
open Core_bench.Std

let read file =
  let open Unix in
  let size = Int64.to_int_exn (stat file).st_size in
  let buf  = String.create size in
  let rec loop pos len fd =
    let n = read ~pos ~len ~buf fd in
    if n = 0 then buf else loop (pos + n) (len - n) fd
  in
  with_file ~mode:[O_RDONLY] file ~f:(fun fd ->
    loop 0 size fd)

let zero =
  String.make (65_536) '\x00'

let make_endian name p input =
  Bench.Test.create ~name (fun () ->
    match Angstrom.(parse_only (skip_many p) (`String zero)) with
    | R.Ok _ -> ()
    | R.Error err -> failwith err)

let make_angstrom_json (test, input) =
  let name = Printf.sprintf "angstrom-json: %s" test in
  Bench.Test.create ~name (fun () ->
    match Angstrom.(parse_only (z RFC7159.json) (`String input)) with
    | R.Ok _ -> ()
    | R.Error err -> failwith err)

let make_yojson (test, input) =
  let name = Printf.sprintf "yojson: %s" test in
  Bench.Test.create ~name (fun () ->
    ignore (Yojson.Basic.from_string input))

let make_ezjsonm (test, input) =
  let name = Printf.sprintf "ezjsonm: %s" test in
  Bench.Test.create ~name (fun () ->
    ignore (Ezjsonm.from_string input))

module Req = Cohttp.Request.Make(Cohttp.String_io.M)
let rec cohttp_reqs sio =
  let open Cohttp.String_io.M in
  Req.read sio
  >>= function
    | `Eof -> ()
    | `Invalid msg -> failwith msg
    | `Ok _        -> cohttp_reqs sio
;;

(* For input files involving trailing numbers, .e.g, [http-requests.txt.100],
 * go into the [benchmarks/data] directory and use the [replicate] script to
 * generate the file, i.e.,
 *
 *   [./replicate http-requests.txt 100]
 *
 *)
let main () =
  (*
  let big_twitter = read "benchmarks/data/twitter.json" in
  let twitter =
    [ "twitter1"   , read "/tmp/attoparsec/benchmarks/json-data/twitter1.json"
    ; "twitter10"  , read "/tmp/attoparsec/benchmarks/json-data/twitter10.json"
    ; "twitter20"  , read "/tmp/attoparsec/benchmarks/json-data/twitter20.json"
    ; "twitter50"  , read "/tmp/attoparsec/benchmarks/json-data/twitter50.json"
    ; "twitter100" , read "/tmp/attoparsec/benchmarks/json-data/twitter100.json"
    ; "twitter-big", big_twitter ] in
*)
  let http100    = read "benchmarks/data/http-requests.txt.100" in
  let http10000    = read "benchmarks/data/http-requests.txt.10000" in
  Command.run (Bench.make_command @@
  (*
    List.map ~f:make_angstrom_json twitter
    @ List.map ~f:make_yojson twitter
    @ List.map ~f:make_ezjsonm twitter
    *)

  [ Bench.Test.create ~name:"angstrom-http.100" (fun () ->
      match Angstrom.(parse_only (z Z.(skip_many RFC2616.request)) (`String http100)) with
      | R.Ok _ -> ()
      | R.Error err -> failwith err)
  ; Bench.Test.create ~name:"angstrom-http.100000" (fun () ->
      match Angstrom.(parse_only (z Z.(skip_many RFC2616.request)) (`String http10000)) with
      | R.Ok _ -> ()
      | R.Error err -> failwith err)
  ; Bench.Test.create ~name:"cohttp.100" (fun () ->
      cohttp_reqs (Cohttp.String_io.open_in http100))
  ; Bench.Test.create ~name:"cohttp.100000" (fun () ->
      cohttp_reqs (Cohttp.String_io.open_in http10000))

    (*
    make_endian "int8  le" Angstrom.Le.int8  zero;
    make_endian "int64 le" Angstrom.Le.int64 zero;
    make_endian "int8  be" Angstrom.Be.int8  zero;
    make_endian "int64 be" Angstrom.Be.int64 zero;
    make_endian "int8  ne" Angstrom.Ne.int8  zero;
    make_endian "int64 ne" Angstrom.Ne.int64 zero;
    *)
  ])

let () = main ()
