open OUnit2
open Enigma

let rotor_I = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let rotor_III = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
let identity = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

let reflector_B = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

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
let suite =
  "Enigma test suite" >::: [
    (* Index tests *)
    index_test "index of A is 0" 'A' 0;
    index_test "index of Z is 25" 'Z' 25;

    (* Rotor mapping tests *)
    map_r_to_l_test "identity A 0" identity 'A' 0 0;
    map_r_to_l_test "identity B 0" identity 'B' 0 0;

    map_r_to_l_test "rotor I A 0" rotor_I 'A' 0 4;
    map_r_to_l_test "rotor I B 0" rotor_I 'B' 0 9;
    map_r_to_l_test "rotor I C 0" rotor_I 'C' 0 10;
    map_r_to_l_test "rotor I Z 0" rotor_I 'Z' 0 10;

    map_r_to_l_test "rotor III O 14" rotor_III 'O' 14 17;

    (* Reverse rotor mapping tests *)
    map_l_to_r_test "identity A 0" identity 'A' 0 0;
    map_l_to_r_test "identity B 0" identity 'B' 0 0;

    map_l_to_r_test "rotor I A 4" rotor_I 'A' 4 0;
    map_l_to_r_test "rotor I A 0" rotor_I 'A' 0 20;
    map_l_to_r_test "rotor I B 9" rotor_I 'B' 9 0;
    map_l_to_r_test "rotor I F 10" rotor_I 'F' 10 14;

    map_l_to_r_test "rotor III O 17" rotor_III 'O' 17 14;

    (* Reflector tests *)
    map_refl_test "identity A" identity 0 0;
    map_refl_test "reflector B A to Y" reflector_B 0 24;
    map_refl_test "reflector B Y to A" reflector_B 24 0;
    map_refl_test "reflector B B to R" reflector_B 1 17;
    map_refl_test "reflector B R to B" reflector_B 17 1;

    (* Plugboard tests *)
    map_plug_test "empty plugboard" [] 'A' 'A';

    map_plug_test "A to Z" [('A', 'Z')] 'A' 'Z';
    map_plug_test "Z to A" [('A', 'Z')] 'Z' 'A';

    map_plug_test "X to Y" [('A', 'Z'); ('X', 'Y')] 'X' 'Y';
    map_plug_test "Y to X" [('A', 'Z'); ('X', 'Y')] 'Y' 'X';

    map_plug_test "unchanged B" [('A', 'Z'); ('X', 'Y')] 'B' 'B';
  ]

let () = run_test_tt_main suite