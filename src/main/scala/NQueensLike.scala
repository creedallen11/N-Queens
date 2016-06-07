/**
  * Created by Creed on 6/7/16.
  */
trait NQueensLike {

  val dim: Int
  val solution: Set[(Int, Int)]

  override def toString(): String = {
    val builder = new StringBuilder((dim + 1) * dim)
    for (y <- 0.to(dim - 1)) {
      for (x <- 0.to(dim - 1)) {
        if (solution.contains((x, y))) {
          builder += 'Q'
        }
        else {
          builder += '.'
        }
      }
      builder ++= "\n"
    }
    builder.toString
  }

  def place(x: Int, y: Int): Option[NQueensLike]

  def solve(): Option[NQueensLike] = {
    if (solution.size == dim) {
      return Some(this)
    }
    else {
      for (x <- 0.to(dim - 1); y <- 0.to(dim - 1)) {
        this.place(x, y) match {
          case Some(newBoard) => newBoard.solve match {
            case Some(b) => return Some(b)
            case None => ()
          }
          case None => ()
        }
      }
      return None
    }
  }

}

object NQueensLike {
  /* Check if piece 1 and piece 2 are in the same row */
  def onSameRow(p1: (Int, Int), p2: (Int, Int)): Boolean = {
    val (x1, y1) = p1
    val (x2, y2) = p2
    y1 == y2
  }

  /* Check if piece 1 and piece 2 are in the same column */
  def onSameCol(p1: (Int, Int), p2: (Int, Int)): Boolean = {
    val (x1, y1) = p1
    val (x2, y2) = p2
    x1 == x2
  }

  /* Check if piece 1 and piece 2 are in the same diagonal */
  def onSameDiagonal(p1: (Int, Int), p2: (Int, Int)): Boolean = {
    val (x1, y1) = p1
    val (x2, y2) = p2
    x1 - y1 == x2 - y2
  }
  /* Check if piece 1 and piece 2 are in the same anti-diagonal */
  def onSameAntiDiagonal(p1: (Int, Int), p2: (Int, Int)): Boolean = {
    val (x1, y1) = p1
    val (x2, y2) = p2
    x1 + y1 == x2 + y2
  }
}