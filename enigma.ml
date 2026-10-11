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
  let offset = index _top_letter in
  let internal_pos = (_input_pos + offset) mod 26 in
  let letter = Char.chr (internal_pos + Char.code 'A') in
  let wire_pos = String.index _wiring letter in
  let outer_index = (wire_pos - offset + 26) mod 26 in
  outer_index

let map_refl _wiring _input_pos =
  index _wiring.[_input_pos]

let rec map_plug plugs c =
  match plugs with
  | [] -> c
  | (a, b) :: rest ->
      if c = a then b
      else if c = b then a
      else map_plug rest c

let rec map_rotors_r_to_l rotors pos =
  match rotors with
  | [] -> pos
  | r :: rest ->
      let next_pos = map_r_to_l r.rotor.wiring r.top_letter pos in
      map_rotors_r_to_l rest next_pos

let rec map_rotors_l_to_r rotors pos =
  match rotors with
  | [] -> pos
  | r :: rest ->
      let next_pos = map_l_to_r r.rotor.wiring r.top_letter pos in
      map_rotors_l_to_r rest next_pos

let cipher_char config c =
  let plugged = map_plug config.plugboard c in
  let input_pos = index plugged in

  let forward = map_rotors_r_to_l (List.rev config.rotors) input_pos in
  let reflected = map_refl config.refl forward in
  let backward = map_rotors_l_to_r config.rotors reflected in

  let output_char = Char.chr (backward + Char.code 'A') in
  map_plug config.plugboard output_char

let step _config =
  failwith "Unimplemented"

let cipher _config _s =
  failwith "Unimplemented"

let hours_worked = 0
