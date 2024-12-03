import scala.annotation.tailrec
import scala.io.Source

trait Security
case object Safe extends Security
case object Unsafe extends Security
object Security {
  def isSafe(s: Security): Boolean = (s == Safe)
}


/*
While the Red-Nosed Reindeer nuclear fusion/fission plant appears to contain no sign of the Chief Historian, the engineers there run up to you as soon as they see you. Apparently, they still talk about the time Rudolph was saved through molecular synthesis from a single electron.

They're quick to add that - since you're already here - they'd really appreciate your help analyzing some unusual data from the Red-Nosed reactor. You turn to check if The Historians are waiting for you, but they seem to have already divided into groups that are currently searching every corner of the facility. You offer to help with the unusual data.

The unusual data (your puzzle input) consists of many reports, one report per line. Each report is a list of numbers called levels that are separated by spaces. For example:

7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
This example data contains six reports each containing five levels.

The engineers are trying to figure out which reports are safe. The Red-Nosed reactor safety systems can only tolerate levels that are either gradually increasing or gradually decreasing. So, a report only counts as safe if both of the following are true:

The levels are either all increasing or all decreasing.
Any two adjacent levels differ by at least one and at most three.
In the example above, the reports can be found safe or unsafe by checking those rules:

7 6 4 2 1: Safe because the levels are all decreasing by 1 or 2.
1 2 7 8 9: Unsafe because 2 7 is an increase of 5.
9 7 6 2 1: Unsafe because 6 2 is a decrease of 4.
1 3 2 4 5: Unsafe because 1 3 is increasing but 3 2 is decreasing.
8 6 4 4 1: Unsafe because 4 4 is neither an increase or a decrease.
1 3 6 7 9: Safe because the levels are all increasing by 1, 2, or 3.
So, in this example, 2 reports are safe.

Analyze the unusual data from the engineers. How many reports are safe?
 */
case class Day2_1() extends Challenge[List[Int], Int]:

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

  def checkReports(in: List[Int]): Security = {

    def diff(curr: Int, prev: Option[Int]): Security = {
      val min = 1
      val max = 3
      prev match {
        case None => Safe
        case Some(p) => {
          val d = (curr - p).abs
          if (d < min || d > max)
            Unsafe
          else
            Safe
        }
      }

    }

    @tailrec
    def decrease(in: List[Int], prev: Option[Int], sec: Security): Security = {
      in match {
        case Nil => sec
        case last::Nil => diff(last, prev)
        case h::tail => if (h <= tail.head)
          Unsafe
        else {
          diff(h, prev) match {
            case Unsafe => Unsafe
            case Safe => decrease(tail, Some(h), Safe)
          }
        }

      }
    }

    @tailrec
    def increase(in: List[Int], prev: Option[Int], sec: Security): Security = {
      in match {
        case Nil => sec
        case last :: Nil => diff(last, prev)
        case h :: tail => if (h >= tail.head)
          Unsafe
        else {
          diff(h, prev) match {
            case Unsafe => Unsafe
            case Safe => increase(tail, Some(h), Safe)
          }
        }
      }
    }

    in match {
      case Nil => Safe
      case last :: Nil => Safe
      case head :: tail =>
        if (head >= tail.head)
          decrease(in, None, Safe)
        else
          increase(in, None, Safe)
    }
  }

  def process(in: List[List[Int]]): Either[Exception, Int] = {

    try {
      val result = in
        .map(checkReports)
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
