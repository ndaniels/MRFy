type aa = char

type query_sequence = string

type score = real
val min_prob = Real.maxFinite : score

type e_scores = score Array.array

datatype node = NODE of {
  m_i: score, m_m: score, m_d: score,
  i_i: score, i_m: score,
  d_m: score, d_d: score,
  m_emission: e_scores, i_emission: e_scores
}

type hmm = node Array.array

datatype state_label = Mat | Ins | Del
val num_labels = 3
type node_count = int
type residue_count = int

fun debug_hmm pre hmm inode =
  println ("[" ^ pre ^ "] Node Idx: " ^ itoa inode
           ^ " (Len: " ^ itoa (Array.length hmm) ^ ")")

fun debug_query pre query ires =
  println ("[" ^ pre ^ "] Res Idx: " ^ itoa ires
           ^ " (Len: " ^ itoa (String.size query) ^ ")")

fun idx_residue query ires =
  let val residue = String.sub (query, ires)
  in  alpha_map residue end
val _ = idx_residue : query_sequence -> residue_count -> int

fun fnode field (NODE node) = field node
fun of_node select hmm j = fnode select (Array.sub (hmm, j))

fun emission_of select hmm query inode ires =
   Array.sub (of_node select hmm inode, idx_residue query ires)

val mat_emission_of = emission_of #m_emission
val ins_emission_of = emission_of #i_emission

exception InvalidTrans of string

fun trans_prob hmm inode from to =
  let val node = Array.sub (hmm, inode)
   in case (from, to)
        of (Mat, Mat) => fnode #m_m node
         | (Mat, Ins) => fnode #m_i node
         | (Mat, Del) => fnode #m_d node
         | (Ins, Mat) => fnode #i_m node
         | (Ins, Ins) => fnode #i_i node
         | (Ins, Del) => raise InvalidTrans "ins -> del"
         | (Del, Mat) => fnode #d_m node
         | (Del, Ins) => raise InvalidTrans "del -> ins"
         | (Del, Del) => fnode #d_d node
   end

fun score_add s1 s2 = s1 + s2
val _ = score_add : score -> score -> score

