val itoa = Int.toString
fun println s = print (s ^ "\n")
fun pintln n = println (itoa n)
fun prealln n = println (Real.toString n)
fun poptrealln NONE = println "None"
  | poptrealln (SOME n) = prealln n
