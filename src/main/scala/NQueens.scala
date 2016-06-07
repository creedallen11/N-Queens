/**
  * Created by Creed on 6/7/16.
  */
class NQueens(val dim: Int, val solution: Set[(Int, Int)]) extends NQueensLike {
  require(dim > 0)

  def place(x: Int, y: Int): Option[NQueens] = {
    if (x >= 0 && x < dim && y >= 0 && y < dim &&
      solution.contains((x, y)) == false &&
      !solution.exists(p => NQueensLike.onSameRow(p, (x, y))) &&
      !solution.exists(p => NQueensLike.onSameCol(p, (x, y))) &&
      !solution.exists(p => NQueensLike.onSameDiagonal(p, (x, y))) &&
      !solution.exists(p => NQueensLike.onSameAntiDiagonal(p, (x, y)))) {
      Some(new NQueens(dim, solution + ((x, y))))
    }
    else {
      None
    }
  }

}
