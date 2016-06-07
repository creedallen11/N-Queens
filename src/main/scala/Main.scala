object Main extends App {

  /* Benchmarker */
  def time[A](body: => A): A = {
    val startTime = System.currentTimeMillis()
    val result = body
    val endTime = System.currentTimeMillis()
    println(s"Time taken: ${endTime - startTime}ms.")
    result
  }

  // Commented out lines take a long, long time
  val empty5 = new NQueens(5, Set())
  println(empty5.solve.get)
  // println(time(new NQueens(10, Set()).solve.get))
  println(time(NQueensOpt.empty(10).solve.get))
  // println(time(NQueensOpt.empty(12).solve.get))
  println(time(NQueensExtra.empty(20).solve.get))
  println(time(NQueensExtra.empty(80).solve.get)) // you can make this much larger
}


