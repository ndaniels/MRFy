fun idx_state Mat = 0
  | idx_state Ins = 1
  | idx_state Del = 2
val _ = idx_state : state_label -> int

type memo_table = {
  hlen: node_count,
  scores: (score option) Array.array
}

fun memo_new nc rc =
  let val hlen = nc + 1
      val slots = hlen * (rc+1) * num_labels
  in {
    hlen = hlen,
    scores = Array.array (slots, NONE)
  } end
val _ = memo_new : node_count -> residue_count -> memo_table

fun memo_cell { hlen = hlen, scores = scores} lab j i =
  idx_state lab + num_labels * (j + hlen * i)
val _ = memo_cell : memo_table -> state_label -> node_count -> residue_count
                    -> int

fun memo_get (mt as { hlen = _, scores = scores}) lab j i =
  Array.sub (scores, memo_cell mt lab j i)
val _ = memo_get : memo_table -> state_label -> node_count -> residue_count
                   -> score option

fun memo_isset mt lab j i = Option.isSome (memo_get mt lab j i)
val _ = memo_isset : memo_table -> state_label -> node_count -> residue_count
                     -> bool

fun memo_set (mt as { hlen = _, scores = scores}) lab j i x =
  let val i = memo_cell mt lab j i
      fun save () = (Array.update (scores, i, SOME x); x)
  in  case Array.sub (scores, i)
        of NONE => save ()
         | SOME y => if x < y then save () else y
  end
val _ = memo_set : memo_table -> state_label -> node_count
                   -> residue_count -> score -> score
