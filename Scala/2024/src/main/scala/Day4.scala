import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.io.Source

/*
"Looks like the Chief's not here. Next!" One of The Historians pulls out a device and pushes the only button on it. After a brief flash, you recognize the interior of the Ceres monitoring station!

As the search for the Chief continues, a small Elf who lives on the station tugs on your shirt; she'd like to know if you could help her with her word search (your puzzle input). She only has to find one word: XMAS.

This word search allows words to be horizontal, vertical, diagonal, written backwards, or even overlapping other words. It's a little unusual, though, as you don't merely need to find one instance of XMAS - you need to find all of them. Here are a few ways XMAS might appear, where irrelevant characters have been replaced with .:


..X...
.SAMX.
.A..A.
XMAS.S
.X....
The actual word search will be full of letters instead. For example:

MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
In this word search, XMAS occurs a total of 18 times; here's the same word search again, but where letters not involved in any XMAS have been replaced with .:

....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX
Take a look at the little Elf's word search. How many times does XMAS appear?
 */
case class Day4(
                 searchTarget: ListMap[Char, Int],
                 rotations: List[(Int, Int)]) extends Challenge[String, Int] {

  override def getInputName: String = "day4.txt"

  override def parse(s: Source): Either[Exception, List[String]] = {
    try {
      Right(s.getLines().toList)
    } catch  {
      case e: Exception => Left(e)
    }
  }

  override def process(in: List[String]): Either[Exception, Int] = {
    val input =  asCharMatrix(in)
    val board = Board(input)
    var counter = 0
    
    val firstLetter = searchTarget.head
    val remainingLetters = searchTarget.drop(1)

    try {
      for (rowIndex <- input.indices)
        for (colIndex <- input(rowIndex).indices) {
          val start =  Location(rowIndex, colIndex)
          if board.getAt(start).contains(firstLetter._1) then
            counter = counter + rotate(rotations, remainingLetters.toList, board, start)
        }
      Right(counter)
    } catch {
      case e: Exception => Left(e)
    }


  }

  def rotate(rotations: List[(Int, Int)], target: List[(Char, Int)], where: Board[Char], start: Location): Int = {
    @tailrec
    def rotateRec(rotations: List[(Int, Int)], target: List[(Char, Int)], count: Int): Int = {
      rotations match {
        case Nil => count
        case (0, 0) :: rest => rotateRec(rest, target, count)
        case rotation :: rest =>
          if (search(target, rotation, where, start))
            rotateRec(rest, target, count + 1)
          else
            rotateRec(rest, target, count)
      }
    }

    rotateRec(rotations, target, 0)
  }

  @tailrec
  private def search(target: List[(Char, Int)], rotation: (Int, Int), where: Board[Char], start: Location): Boolean =
    rotation match {
      case (0, 0) => false // no rotation, actual location of starting character
      case (rx, ry) => target match {
        case Nil => true // end of target, all good
        case current::rest =>
          val pos = start.translate(rotation, current._2)
          if !(where getAt pos).contains(current._1) then
            false
          else
            search(rest, rotation, where, start)

      }
    }

  private def asCharMatrix(in: List[String]): Array[Array[Char]] = {
    in match {
      case Nil => Array.empty
      case _ => in.map(s => s.toCharArray).toArray
    }
  }
  
}

object Day4 {
  def part1Factory(): Day4 = {
    Day4(
      ListMap(
        ('X', 0),
        ('M', 1),
        ('A', 2),
        ('S', 3),
      ),
      pairs(Array(-1, 0, 1)).toList
    )
  }

  def part2Factory(): Day4 = {
    // TODO: still needs to include only X-shaped MASes in the solution
    Day4(
      ListMap(
        ('A', 0),
        ('M', -1),
        ('S', 1),
      ),
      pairs(Array(-1, 1)).toList
    )
  }

  private def pairs[T](source: Seq[T]): Seq[(T, T)] =
    for {
      a <- source
      b <- source
    } yield (a, b)
  
}

