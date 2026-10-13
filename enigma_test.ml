open OUnit2
open Enigma

(* Historical rotor and reflector wiring used by the tests. *)
let rotor_I = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let rotor_II = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
let rotor_III = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
let reflector_B = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
let identity = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

(* Construct rotor configurations without repeating record definitions. *)
let make_rotor wiring turnover top_letter =
  { rotor = { wiring; turnover }; top_letter }

let rotor_i_at_a = make_rotor rotor_I 'Q' 'A'
let rotor_ii_at_a = make_rotor rotor_II 'E' 'A'
let rotor_iii_at_a = make_rotor rotor_III 'V' 'A'

let make_config rotors =
  { refl = reflector_B; rotors; plugboard = [] }

let full_config = {
  refl = reflector_B;
  rotors = [
    make_rotor rotor_I 'Q' 'F';
    make_rotor rotor_II 'E' 'U';
    make_rotor rotor_III 'V' 'N';
  ];
  plugboard = [('A', 'Z')];
}

(* Small helpers keep test cases easy to read. *)
let index_test name input expected =
  name >:: fun _ ->
    assert_equal expected (index input)

let map_r_to_l_test name wiring top_letter input expected =
  name >:: fun _ ->
    assert_equal expected (map_r_to_l wiring top_letter input)

let map_l_to_r_test name wiring top_letter input expected =
  name >:: fun _ ->
    assert_equal expected (map_l_to_r wiring top_letter input)

let map_refl_test name wiring input expected =
  name >:: fun _ ->
    assert_equal expected (map_refl wiring input)

let map_plug_test name plugs input expected =
  name >:: fun _ ->
    assert_equal expected (map_plug plugs input)

let cipher_char_test name config input expected =
  name >:: fun _ ->
    assert_equal expected (cipher_char config input)

let cipher_test name config input expected =
  name >:: fun _ ->
    assert_equal expected (cipher config input)

(** Read a rotor list's top letters from left to right. *)
let rotor_positions rotors =
  String.concat ""
    (List.map (fun r -> String.make 1 r.top_letter) rotors)

(* The three-letter strings in stepping tests are left-to-right positions. *)
let step_test name start expected =
  name >:: fun _ ->
    let config = {
      refl = identity;
      rotors = [
        make_rotor rotor_III 'V' start.[0];
        make_rotor rotor_II 'E' start.[1];
        make_rotor rotor_I 'Q' start.[2];
      ];
      plugboard = [];
    } in
    let result = step config in
    assert_equal ~printer:(fun s -> s) expected (rotor_positions result.rotors)

let suite =
  "Enigma test suite" >::: [
    (* Alphabet indices: beginning, middle, and end. *)
    index_test "index of A is 0" 'A' 0;
    index_test "index of Z is 25" 'Z' 25;
    index_test "index of B is 1" 'B' 1;
    index_test "index of M is 12" 'M' 12;
    index_test "index of Y is 24" 'Y' 24;

    (* Forward rotor mapping: identity, rotation, and historical wiring. *)
    map_r_to_l_test "forward identity at A maps 0 to 0" identity 'A' 0 0;
    map_r_to_l_test "forward identity at B maps 0 to 0" identity 'B' 0 0;
    map_r_to_l_test "forward rotor I at A maps 0 to 4" rotor_I 'A' 0 4;
    map_r_to_l_test "forward rotor I at B maps 0 to 9" rotor_I 'B' 0 9;
    map_r_to_l_test "forward rotor I at C maps 0 to 10" rotor_I 'C' 0 10;
    map_r_to_l_test "forward rotor I at Z wraps input 0" rotor_I 'Z' 0 10;
    map_r_to_l_test "forward rotor III at O maps 14 to 17" rotor_III 'O' 14 17;

    (* Reverse rotor mapping. *)
    map_l_to_r_test "reverse identity at A maps 0 to 0" identity 'A' 0 0;
    map_l_to_r_test "reverse identity at B maps 0 to 0" identity 'B' 0 0;
    map_l_to_r_test "reverse rotor I at A maps 4 to 0" rotor_I 'A' 4 0;
    map_l_to_r_test "reverse rotor I at A maps 0 to 20" rotor_I 'A' 0 20;
    map_l_to_r_test "reverse rotor I at B maps 9 to 0" rotor_I 'B' 9 0;
    map_l_to_r_test "reverse rotor I at F maps 10 to 14" rotor_I 'F' 10 14;
    map_l_to_r_test "reverse rotor III at O maps 17 to 14" rotor_III 'O' 17 14;

    (* Reflector symmetry: A <-> Y, B <-> R. *)
    map_refl_test "identity A" identity 0 0;
    map_refl_test "reflector B A to Y" reflector_B 0 24;
    map_refl_test "reflector B Y to A" reflector_B 24 0;
    map_refl_test "reflector B B to R" reflector_B 1 17;
    map_refl_test "reflector B R to B" reflector_B 17 1;

    (* Plugboard: both directions, multiple pairs, and unchanged letters. *)
    map_plug_test "empty plugboard" [] 'A' 'A';
    map_plug_test "A to Z" [('A', 'Z')] 'A' 'Z';
    map_plug_test "Z to A" [('A', 'Z')] 'Z' 'A';
    map_plug_test "X to Y" [('A', 'Z'); ('X', 'Y')] 'X' 'Y';
    map_plug_test "Y to X" [('A', 'Z'); ('X', 'Y')] 'Y' 'X';
    map_plug_test "unchanged B" [('A', 'Z'); ('X', 'Y')] 'B' 'B';

    (* Static cipher: different numbers of rotors, without stepping. *)
    cipher_char_test "no rotors A" (make_config []) 'A' 'Y';
    cipher_char_test "one rotor A" (make_config [rotor_i_at_a]) 'A' 'H';
    cipher_char_test "two rotors G" (make_config [rotor_i_at_a; rotor_ii_at_a]) 'G' 'V';
    cipher_char_test "three rotors G" (make_config [rotor_i_at_a; rotor_ii_at_a; rotor_iii_at_a]) 'G' 'P';
    cipher_char_test "three rotors A" (make_config [rotor_i_at_a; rotor_ii_at_a; rotor_iii_at_a]) 'A' 'U';
    cipher_char_test "three rotors B" (make_config [rotor_i_at_a; rotor_ii_at_a; rotor_iii_at_a]) 'B' 'E';

    (* Stepping: regular movement, turnover, and double-stepping. *)
    step_test "KDO to KDP" "KDO" "KDP";
    step_test "KDP to KDQ" "KDP" "KDQ";
    step_test "KDQ to KER" "KDQ" "KER";
    step_test "KER to LFS" "KER" "LFS";
    step_test "LFS to LFT" "LFS" "LFT";
    step_test "VDP to VDQ" "VDP" "VDQ";
    step_test "VDQ to VER" "VDQ" "VER";
    step_test "VER to WFS" "VER" "WFS";
    step_test "rightmost wraps Z to A" "ZZZ" "ZZA";

    (* Full cipher: messages, decryption, empty input, and repeated letters. *)
    cipher_test "full Enigma cipher" full_config "YNGXQ" "OCAML";
    cipher_test "empty string" full_config "" "";
    cipher_test "single character" full_config "A" "N";
    cipher_test "decrypt OCAML" full_config "OCAML" "YNGXQ";
    cipher_test "repeated letters" full_config "AAAAA" "NLGPZ";
    cipher_test "identity machine HELLO"
      { refl = identity; rotors = []; plugboard = [] }
      "HELLO" "HELLO";
  ]

let () = run_test_tt_main suite
