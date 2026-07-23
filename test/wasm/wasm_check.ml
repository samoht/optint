(* Runs optint's core round-trip identities under wasm_of_ocaml, mirroring the
   package's own "identity with int32" / "identity with int" fuzz properties.
   These hold on every backend, so a wrong emulated computation on the 31-bit
   wasm target fails the job (the workflow also checks for the integer-overflow
   warning of #28). The int32 vectors include the sign-bit boundaries the
   constant fix touches: 0x7FFFFFFF, 0x80000000, 0xFFFFFFFF, 0x40000000. *)

let fail name = Printf.eprintf "FAIL: %s\n" name; exit 1

let i32s =
  [ 0l; 1l; -1l; 2l; -2l; Int32.max_int; Int32.min_int;
    0x7FFFFFFFl; 0x80000000l; 0xFFFFFFFFl; 0x40000000l; 0xC0000000l;
    0x00010000l; 0x0000FFFFl; 123456789l; -123456789l ]

let ints = [ 0; 1; -1; 42; -42; 1000; -1000; 0x3FFFFFFF; -0x3FFFFFFF ]

module Check (I : sig
  type t
  val of_int : int -> t
  val to_int : t -> int
  val of_int32 : int32 -> t
  val to_int32 : t -> int32
  val equal : t -> t -> bool
end) (M : sig val name : string end) = struct
  let () =
    List.iter (fun x ->
      if not (Int32.equal (I.to_int32 (I.of_int32 x)) x) then
        fail (Printf.sprintf "%s: to_int32 (of_int32 %ld)" M.name x);
      if not (I.equal (I.of_int32 x) (I.of_int32 x)) then
        fail (Printf.sprintf "%s: equal (of_int32 %ld)" M.name x))
      i32s;
    List.iter (fun x ->
      if I.to_int (I.of_int x) <> x then
        fail (Printf.sprintf "%s: to_int (of_int %d)" M.name x))
      ints
end

module _ = Check (Optint) (struct let name = "Optint" end)
module _ = Check (Optint.Int63) (struct let name = "Int63" end)

let () = print_endline "optint wasm: ok"
