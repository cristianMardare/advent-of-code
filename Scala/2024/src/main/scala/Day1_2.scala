import scala.annotation.tailrec
import scala.io.Source

/*
Your analysis only confirmed what everyone feared: the two lists of location IDs are indeed very different.

Or are they?

The Historians can't agree on which group made the mistakes or how to read most of the Chief's handwriting, but in the commotion you notice an interesting detail: a lot of location IDs appear in both lists! Maybe the other numbers aren't location IDs at all but rather misinterpreted handwriting.

This time, you'll need to figure out exactly how often each number from the left list appears in the right list. Calculate a total similarity score by adding up each number in the left list after multiplying it by the number of times that number appears in the right list.

Here are the same example lists again:

3   4
4   3
2   5
1   3
3   9
3   3
For these example lists, here is the process of finding the similarity score:

The first number in the left list is 3. It appears in the right list three times, so the similarity score increases by 3 * 3 = 9.
The second number in the left list is 4. It appears in the right list once, so the similarity score increases by 4 * 1 = 4.
The third number in the left list is 2. It does not appear in the right list, so the similarity score does not increase (2 * 0 = 0).
The fourth number, 1, also does not appear in the right list.
The fifth number, 3, appears in the right list three times; the similarity score increases by 9.
The last number, 3, appears in the right list three times; the similarity score again increases by 9.
So, for these example lists, the similarity score at the end of this process is 31 (9 + 4 + 0 + 0 + 9 + 9).

Once again consider your left and right lists. What is their similarity score?
 */
case class Day1_2() extends Challenge[(Int, Int), Int]:

  def getInputName: String = "day1-input2.txt"

  def parse(s: Source): Either[Exception, List[(Int,Int)]] = {
    try {
      val result = s.getLines()
        .map(s => s.split(" {3}") match {
          case Array(i, j) => (i.toInt, j.toInt)
        })
        .toList

      Right(result)

    } catch  {
      case e: Exception => Left(e)
    }


  }

  def process(in: List[(Int, Int)]): Either[Exception, Int] = {

    @tailrec
    def similarity(in: List[Int], m: Map[Int, Int], out: List[Int]): List[Int] = {
      in match {
        case Nil => out
        case h::t => similarity(t, m, out ++ List(h * m.getOrElse(h, 0)))
      }
    }

    @tailrec
    def count(in: List[Int], out: Map[Int, Int]): Map[Int, Int] = {
      in match {
        case Nil => out
        case h::t => {
          out contains h match {
            case true => count(t, out + (h -> (out(h) + 1)))  // increment counter for existing value in map
            case false => count(t, out + (h -> 1))  // add new value to map
          }
        }
      }
    }

    try {
      val (input, mapOfValues) =
        in.unzip match {
          case (l1: List[Int], l2: List[Int]) => (l1.sorted, count(l2, Map()))
        }

      val result = similarity(input, mapOfValues, List()).sum
      Right(result)
    } catch {
      case e: Exception => Left(e)
    }
  }