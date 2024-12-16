import scala.reflect.ClassTag

case class Board[T:ClassTag](in: Array[Array[T]]) {
  def getAt(pos: Location): Option[T] = {
    if isInBounds(pos) then
      Some(in(pos.x)(pos.y))
    else
      None
  }

  def isInBounds(location: Location): Boolean = {
    if (location._1 < 0 || location._2 < 0)
      false
    else if (location._1 < in.length && location._2 < in(0).length)
      true
    else false
  }

  def replace(where: Location, _with: T): Board[T] =
    if (isInBounds(where))
      in(where.x)(where.y) = _with
      this
    else
      this

  def copy(): Board[T] =
    Board[T](
      in.map(_.clone)
    )

  def count(op: T => Boolean): Int = {
    var counter = 0
    for (i <- in.indices)
      for (j <- in.head.indices)
        if (op apply in(i)(j))
          counter += 1

    counter
  }
}