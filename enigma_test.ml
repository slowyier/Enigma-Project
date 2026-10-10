open OUnit2
open Enigma

let rotor_I = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let rotor_III = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
let identity = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

let index_test name input expected =
  name >:: fun _ ->
    assert_equal expected (index input)

let map_r_to_l_test name wiring top_letter input expected =
  name >:: fun _ ->
    assert_equal expected (map_r_to_l wiring top_letter input)

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
  ]

let () = run_test_tt_main suite