exception InvalidResidue of char

(* From MRFy: *)
(* aminoList = "ACDEFGHIKLMNPQRSTVWYX" *)
fun alpha_map #"A" = 0
  | alpha_map #"B" = raise InvalidResidue #"B"
  | alpha_map #"C" = 1
  | alpha_map #"D" = 2
  | alpha_map #"E" = 3
  | alpha_map #"F" = 4
  | alpha_map #"G" = 5
  | alpha_map #"H" = 6
  | alpha_map #"I" = 7
  | alpha_map #"J" = raise InvalidResidue #"J"
  | alpha_map #"K" = 8
  | alpha_map #"L" = 9
  | alpha_map #"M" = 10
  | alpha_map #"N" = 11
  | alpha_map #"O" = raise InvalidResidue #"O"
  | alpha_map #"P" = 12
  | alpha_map #"Q" = 13
  | alpha_map #"R" = 14
  | alpha_map #"S" = 15
  | alpha_map #"T" = 16
  | alpha_map #"U" = raise InvalidResidue #"U"
  | alpha_map #"V" = 17
  | alpha_map #"W" = 18
  | alpha_map #"X" = 20 (* Tricky! X comes after Y! X is a wildcard residue. *)
  | alpha_map #"Y" = 19
  | alpha_map #"Z" = raise InvalidResidue #"Z"
  | alpha_map c    = raise InvalidResidue c
