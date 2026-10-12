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

let next_letter c =
  Char.chr (Char.code 'A' + (index c + 1) mod 26)

let rotate_rotor r =
  { r with top_letter = next_letter r.top_letter }
let step _config =
  let at_turnover r =
    r.top_letter = r.rotor.turnover
  in

  let rec step_rotors is_leftmost rotors =
    match rotors with
    | [] -> []
    | [r] -> [rotate_rotor r]
    | r :: ((right :: _) as rest) ->
        let should_rotate =
          at_turnover right ||
          (not is_leftmost && at_turnover r)
        in
        let new_r =
          if should_rotate then rotate_rotor r else r
        in
        new_r :: step_rotors false rest
  in

  { _config with rotors = step_rotors true _config.rotors }

let cipher config s =
  let rec aux current_config i acc =
    if i >= String.length s then
      String.concat "" (List.rev acc)
    else
      let new_config = step current_config in
      let encrypted = cipher_char new_config s.[i] in
      aux new_config (i + 1) (String.make 1 encrypted :: acc)
  in
  aux config 0 []

let hours_worked = 9
