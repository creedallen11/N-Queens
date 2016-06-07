/**
  * Created by Creed on 6/7/16.
  */

/* An Queens that returns Options instead. */
class NQueensOpt(val dim: Int, val occupiedRows: Set[Int], val occupiedCols: Set[Int], val occupiedDiags: Set[Int],
                 val occupiedAntiDiags: Set[Int], val solution: Set[(Int, Int)]) extends NQueensLike {

  def place(x: Int, y: Int): Option[NQueensOpt] = {
    if (x >= 0 && x < dim && y >= 0 && y < dim &&
      !occupiedRows.contains(y) &&
      !occupiedCols.contains(x) &&
      !occupiedDiags.contains(x - y) &&
      !occupiedAntiDiags.contains(x + y)) {
      Some(new NQueensOpt(dim = dim,
        solution = solution + ((x, y)),
        occupiedRows = occupiedRows + y,
        occupiedCols = occupiedCols + x,
        occupiedDiags = occupiedDiags + (x - y),
        occupiedAntiDiags = occupiedAntiDiags + (x + y)))
    }
    else {
      None
    }
  }

}

/* Companion object containing static methods */
object NQueensOpt {

  def empty(dim: Int): NQueensOpt = {
    new NQueensOpt(dim = dim,
      solution = Set(),
      occupiedRows = Set(),
      occupiedCols = Set(),
      occupiedDiags = Set(),
      occupiedAntiDiags = Set())
  }
}