/**
  * Created by Creed on 6/7/16.
  */
// Due to changes in solve(), we can't inherit form NQueensLike anymore.
// How would you fix this?
class NQueensExtra(val dim: Int,
                   val available: List[(Int, Int)],
                   val solution: Set[(Int, Int)]) {

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

  def place(x: Int, y: Int): NQueensExtra = {
    require (available.contains((x, y)))
    def isOccupied(p: (Int, Int)): Boolean = {
      val (x1, y1) = p
      x1 == x || y1 == y || x1 + y1 == x + y  || x1 - y1 == x - y
    }
    new NQueensExtra(dim,
      available.filterNot(isOccupied),
      solution + ((x, y)))
  }

  def solve(): Option[NQueensExtra] = {
    if (available.size == 0) {
      assert(NQueensExtra.isValidSolution(dim, solution))
      return Some(this)
    }
    else {
      for ((x, y) <- this.available) {
        this.place(x, y).solve match {
          case Some(b) => return Some(b)
          case None => ()
        }
      }
    }
    return None
  }

}

object NQueensExtra {

  def empty(dim: Int) = {
    val available = for (x <- 0.until(dim); y <- 0.until(dim)) yield { (x, y) }
    new NQueensExtra(dim, available.toList, Set())
  }

  def allPairs[A](lst: List[A]): List[(A, A)] = {
    lst.tails.map {
      case Nil => Nil
      case x :: ys => ys.map(y => (x, y))
    }.flatten.toList
  }

  def isValidSolution(dim: Int, solution: Set[(Int, Int)]): Boolean = {
    !allPairs(solution.toList).exists(arg => {
      val (p1, p2) = arg
      NQueensLike.onSameRow(p1, p2) ||
      NQueensLike.onSameCol(p1, p2) ||
      NQueensLike.onSameDiagonal(p1, p2) ||
      NQueensLike.onSameAntiDiagonal(p1, p2)
    })
  }

}