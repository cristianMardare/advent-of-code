import scala.annotation.tailrec
import scala.io.Source


/*
The engineers are surprised by the low number of safe reports until they realize they forgot to tell you about the Problem Dampener.

The Problem Dampener is a reactor-mounted module that lets the reactor safety systems tolerate a single bad level in what would otherwise be a safe report. It's like the bad level never happened!

Now, the same rules apply as before, except if removing a single level from an unsafe report would make it safe, the report instead counts as safe.

More of the above example's reports are now safe:

7 6 4 2 1: Safe without removing any level.
1 2 7 8 9: Unsafe regardless of which level is removed.
9 7 6 2 1: Unsafe regardless of which level is removed.
1 3 2 4 5: Safe by removing the second level, 3.
8 6 4 4 1: Safe by removing the third level, 4.
1 3 6 7 9: Safe without removing any level.
Thanks to the Problem Dampener, 4 reports are actually safe!
 */
case class Day2_2() extends Challenge[List[Int], Int]:

  def getInputName: String = "day2.txt"

  def parse(s: Source): Either[Exception, List[List[Int]]] = {
    try {
      val result = s.getLines()
        .map(s => s.split(" ").map(s => s.toInt).toList
        )
        .toList

      Right(result)

    } catch  {
      case e: Exception => Left(e)
    }


  }

  def checkReport(in: List[Int]): Security = {

    @tailrec
    def next(in: List[Int], comparison: (Int, Int) => Boolean): Security = {
      in match {
        case Nil =>  Safe
        case last::Nil =>  Safe
        case head::tail =>
          val isSafe = comparison(head, tail.head)
          if isSafe then
            next(tail, comparison)
          else
            Unsafe
      }
    }

    def checkReportInternal(in: List[Int]): Security = {
      val increaseComp = (a: Int, b: Int) => (1 <= (b - a)) && ((b - a) <= 3)
      val decreaseComp = (a: Int, b: Int) => (1 <= (a - b)) && ((a - b) <= 3)

      in match {
        case Nil => Safe
        case last :: Nil => Safe
        case head :: tail =>
          if head < tail.head then
            next(in, increaseComp)
          else
            next(in, decreaseComp)

      }
    }

    @tailrec
    def checkSubReport(head: List[Int], tail: List[Int]): Security = {
      (head, tail) match {
        case (_, Nil) => Unsafe
        case (_, skip :: rest) => checkReportInternal(head ::: rest) match {
          case Safe => Safe
          case Unsafe => checkSubReport(head ::: List(skip), rest)
        }
      }
    }

    checkReportInternal(in) match {
      case Safe => Safe
      case Unsafe => checkSubReport(Nil, in)
    }

  }

  def process(in: List[List[Int]]): Either[Exception, Int] = {

    try {
      val result = in
        .map(checkReport)
        .map(s => {
          Console.println(s)
          s
        })
        .count(Security.isSafe)

      Right(result)
    } catch {
      case e: Exception => Left(e)
    }
  }
