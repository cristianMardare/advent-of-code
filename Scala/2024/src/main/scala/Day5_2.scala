import scala.annotation.tailrec

class Day5_2 extends BaseDay5 {
  @tailrec
  private def processRec(in: List[Int], pagesBefore: Map[Int, Set[Int]]): Boolean =
    in match {
      case Nil => true
      case head :: tail => pagesBefore get head match
        case None => processRec(tail, pagesBefore)
        case Some(pages) =>
          if (tail.count(i => pages contains i) > 0)
            false
          else processRec(tail, pagesBefore)
    }
    
  override def algo(in: List[List[Int]], orderingMap: Map[Int, Set[Int]], middleFn: Array[Int] => Int): Int = {
    {
      def lt(a: Int, b: Int): Boolean = {
        orderingMap get b match {
          case None => false
          case Some(items) =>
            items contains a
        }
      }
      in
        .map(u => (u, processRec(u, orderingMap)))
        .filter((_, v) => !v)
        .map((u, _) => u.sortWith(lt).toArray)
        .map(middleFn)
        .sum()
    }
  }
}
