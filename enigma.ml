type rotor = {
  wiring : string;
  turnover : char;
}

type oriented_rotor = {
  rotor : rotor;
  top_letter : char;
}

type config = {
  refl : string;
  rotors : oriented_rotor list;
  plugboard : (char * char) list;
}

let index _c =
  Char.code _c - Char.code 'A'

let map_r_to_l _wiring _top_letter _input_pos =
  let offset = index _top_letter in
  let internal_wire_pos = (_input_pos + offset) mod 26 in
  let internal_wire_index = index _wiring.[internal_wire_pos] in
  let outer_index = (internal_wire_index - offset + 26) mod 26 in
  outer_index

let map_l_to_r _wiring _top_letter _input_pos =
  failwith "Unimplemented"

let map_refl _wiring _input_pos =
  failwith "Unimplemented"

let map_plug _plugs _c =
  failwith "Unimplemented"

let cipher_char _config _c =
  failwith "Unimplemented"

let step _config =
  failwith "Unimplemented"

let cipher _config _s =
  failwith "Unimplemented"

let hours_worked = 0
