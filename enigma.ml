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

(** Convert an uppercase letter to an alphabet position (A = 0, Z = 25). *)
let index c =
  Char.code c - Char.code 'A'

(** Convert an alphabet position from 0 to 25 back to a letter. *)
let letter_of_index pos =
  Char.chr (Char.code 'A' + pos)

(** Map a position through a rotor from right to left.
    Shift into the rotor, follow its wiring, then shift back. *)
let map_r_to_l wiring top_letter input_pos =
  let offset = index top_letter in
  let internal_pos = (input_pos + offset) mod 26 in
  let mapped_pos = index wiring.[internal_pos] in
  (mapped_pos - offset + 26) mod 26

(** Map a position through a rotor from left to right.
    This looks up the inverse of the rotor wiring. *)
let map_l_to_r wiring top_letter input_pos =
  let offset = index top_letter in
  let internal_pos = (input_pos + offset) mod 26 in
  let letter = letter_of_index internal_pos in
  let wire_pos = String.index wiring letter in
  (wire_pos - offset + 26) mod 26

(** A reflector does not rotate; it maps directly through its wiring. *)
let map_refl wiring input_pos =
  index wiring.[input_pos]

(** Swap a letter with its plugboard partner, or leave it unchanged. *)
let rec map_plug plugs c =
  match plugs with
  | [] -> c
  | (a, b) :: rest ->
      if c = a then b
      else if c = b then a
      else map_plug rest c

(** Pass a signal through rotors in the given list order, right to left. *)
let rec map_rotors_r_to_l rotors pos =
  match rotors with
  | [] -> pos
  | r :: rest ->
      let next_pos = map_r_to_l r.rotor.wiring r.top_letter pos in
      map_rotors_r_to_l rest next_pos

(** Pass a signal through rotors in the given list order, left to right. *)
let rec map_rotors_l_to_r rotors pos =
  match rotors with
  | [] -> pos
  | r :: rest ->
      let next_pos = map_l_to_r r.rotor.wiring r.top_letter pos in
      map_rotors_l_to_r rest next_pos

(** Encrypt one letter using the current rotor positions (without stepping). *)
let cipher_char config c =
  let plugged = map_plug config.plugboard c in
  let input_pos = index plugged in

  (* Rotors are stored left to right, so the forward path uses List.rev. *)
  let forward = map_rotors_r_to_l (List.rev config.rotors) input_pos in
  let reflected = map_refl config.refl forward in
  let backward = map_rotors_l_to_r config.rotors reflected in

  let output_char = letter_of_index backward in
  map_plug config.plugboard output_char

(** Advance a letter by one position, wrapping Z back to A. *)
let next_letter c =
  letter_of_index ((index c + 1) mod 26)

(** Return a new oriented rotor advanced by one letter. *)
let rotate_rotor r =
  { r with top_letter = next_letter r.top_letter }

(** Step the rotors without changing the original configuration.
    All turnover checks use positions before this step. *)
let step config =
  let at_turnover r =
    r.top_letter = r.rotor.turnover
  in

  let rec step_rotors is_leftmost rotors =
    match rotors with
    | [] -> []
    | [r] -> [rotate_rotor r]  (* The rightmost rotor always moves. *)
    | r :: ((right :: _) as rest) ->
        (* Move when the right neighbor is at its notch, or this
           non-leftmost rotor is at its own notch (double-stepping). *)
        let should_rotate =
          at_turnover right ||
          (not is_leftmost && at_turnover r)
        in
        let new_r =
          if should_rotate then rotate_rotor r else r
        in
        new_r :: step_rotors false rest
  in

  { config with rotors = step_rotors true config.rotors }

(** Encrypt a string by stepping before encoding each character.
    Pass the new configuration to the next recursive call. *)
let cipher config s =
  let rec encrypt current_config pos reversed_chars =
    if pos >= String.length s then
      String.concat "" (List.rev reversed_chars)
    else
      let next_config = step current_config in
      let encrypted = cipher_char next_config s.[pos] in
      let new_chars = String.make 1 encrypted :: reversed_chars in
      encrypt next_config (pos + 1) new_chars
  in
  encrypt config 0 []

let hours_worked = 11
