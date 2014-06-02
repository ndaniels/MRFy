exception ViterbiInvalidState of string

fun viterbi hmm query =
  let val (hlen, qlen) = (Array.length hmm, String.size query)
      val mt = memo_new hlen qlen
      fun vee_memo lab j i =
        (case memo_get mt lab j i
           of (SOME s) => s
            | NONE => memo_set mt lab j i (vee lab j i))
      and vee Mat 0 0 = raise ViterbiInvalidState "Mat/0/0"
        | vee Ins 0 0 = of_node #m_i hmm 0
        | vee Ins 0 i = vee_score Ins 0 i Ins
        | vee Mat 1 0 = of_node #m_m hmm 0
        | vee Mat 1 i = let val is = vee_score Ins 1 i Mat
                            val ds = vee_score Del 1 i Mat
                         in Real.min (is, ds) end
        | vee Del 1 0 = of_node #m_d hmm 0
        | vee Del 1 i = min_prob
        | vee Mat j i = let val ms = vee_score Mat j i Mat
                            val is = vee_score Ins j i Mat
                            val ds = vee_score Del j i Mat
                         in Real.min (ms, Real.min (is, ds)) end
        | vee Ins j i = let val ms = vee_score Mat j i Ins
                            val is = vee_score Ins j i Ins
                         in Real.min (ms, is) end
        | vee Del j i = let val ms = vee_score Mat j i Del
                            val ds = vee_score Del j i Del
                         in Real.min (ms, ds) end
      and vee_score lab j i lab_right =
        let val j = if lab_right <> Ins then j - 1 else j
            val i = if lab <> Del then i - 1 else i
            fun semit () =
              let val s = trans_prob hmm j lab lab_right
              in (case lab
                    of Mat => score_add s (mat_emission_of hmm query j i)
                     | Ins => score_add s (ins_emission_of hmm query j i)
                     | Del => s)
              end
         in if j < 0 orelse i < 0
              then min_prob
              else score_add (semit ()) (vee_memo lab j i)
         end
  in vee Mat hlen qlen end
val _ = viterbi : hmm -> query_sequence -> score

