infixr 0 $
fun f $ x = f x

fun repeat 1 x = x ()
  | repeat n x = (x (); repeat (n-1) x)
val _ = repeat : int -> (unit -> 'a) -> 'a

fun main (prog_name, args) =
  let
    val num_passes =
      case args
        of (x::_) => Option.valOf $ Int.fromString x
         | [] => 1
    fun run_model hmm = map (run_query hmm) input_queries
    and run_query hmm query =
      let val score = repeat num_passes (run_vee hmm query)
      in  println $ "Model (" ^ (itoa (Array.length hmm)) ^ " nodes) "
                    ^ "query (" ^ (itoa (String.size query)) ^ " residues) "
                    ^ "Score: " ^ (Real.toString score)
      end
    and run_vee hmm query () = viterbi hmm query
  in map run_model input_hmms end

val _ = main (CommandLine.name (), CommandLine.arguments ())

