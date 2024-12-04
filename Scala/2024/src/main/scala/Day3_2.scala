import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex


/*
As you scan through the corrupted memory, you notice that some of the conditional statements are also still intact. If you handle some of the uncorrupted conditional statements in the program, you might be able to get an even more accurate result.

There are two new instructions you'll need to handle:

The do() instruction enables future mul instructions.
The don't() instruction disables future mul instructions.
Only the most recent do() or don't() instruction applies. At the beginning of the program, mul instructions are enabled.

For example:

xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
This corrupted memory is similar to the example from before, but this time the mul(5,5) and mul(11,8) instructions are disabled because there is a don't() instruction before them. The other mul instructions function normally, including the one at the end that gets re-enabled by a do() instruction.

This time, the sum of the results is 48 (2*4 + 8*5).
 */
sealed trait Command
case object Dont extends Command
case object Do extends Command
case class Multiply(first: Int, second: Int) extends Command

case class Day3_2() extends Challenge[String, Int] {

  val allPatterns: Regex = "mul\\((\\d+),(\\d+)\\)|don't\\(\\)|do\\(\\)".r
  val mulPattern: Regex = "mul\\((\\d+),(\\d+)\\)".r
  val doPattern: Regex = "(do\\(\\))".r
  val dontPattern: Regex = "(don't\\(\\))".r

  override def getInputName: String = "day3.txt"

  override def parse(s: Source): Either[Exception, List[String]] = {
    try {
      Right(List(s.mkString))
    } catch  {
      case e: Exception => Left(e)
    }
  }

  override def process(in: List[String]): Either[Exception, Int] = {
    val input = in.head

    try {
      val commands: List[Command] =
        allPatterns
          .findAllIn(input)
          .map(toCommand)
          .toList

      val result = pruneInactive(commands, List(), true)
        .map(m => m.first * m.second)
        .sum

      Right(result)
    } catch {
      case e: Exception => Left(e)
    }


  }

  private def toCommand(in: String): Command =
    in match {
      case "don't()" => Dont
      case "do()" => Do
      case mulPattern(n1, n2) => Multiply(n1.toInt, n2.toInt)
    }

  @tailrec
  private def pruneInactive(in: List[Command], out: List[Multiply], active: Boolean): List[Multiply] = {
    in match {
      case Nil => out
      case head::tail => (head, active) match {
        case (Dont, _) => pruneInactive(tail, out, false)
        case (Do, _) => pruneInactive(tail, out, true)
        case (m @ Multiply(_, _), true) => pruneInactive(tail, out ::: List(m), true)
        case (Multiply(_, _), false) => pruneInactive(tail, out, false)
      }
    }
  }
}
