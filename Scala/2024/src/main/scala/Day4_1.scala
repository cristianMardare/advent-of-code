import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.io.Source


case class Location(x: Int, y: Int) {
  def translate(vector: (Int, Int), offset: Int): Location =
    Location(
      x + vector._1 * offset,
      y + vector._2 * offset
    )
}
case class Board[T](in: Array[Array[T]]) {
  def getAt(pos: Location): Option[T] = {
    if isInBounds(pos) then
      Some(in(pos.x)(pos.y))
    else
      None
  }

  private def isInBounds(location: Location): Boolean = {
    if (location._1 < 0 || location._2 < 0)
      false
    else if (location._1 < in.length && location._2 < in(0).length)
      true
    else false
  }
}

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
case class Day4_1() extends Challenge[String, Int] {

  override def getInputName: String = "day4.txt"
  val searchTarget: ListMap[Char, Int] = ListMap(
    ('X', 0),
      ('M', 1),
      ('A', 2),
      ('S', 3),
  )
  val rotations: List[(Int, Int)] = pairs(Array(-1, 0, 1)).toList

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

  private def pairs[T](source: Seq[T]): Seq[(T, T)] =
    for {
      a <- source
      b <- source
    } yield (a, b)
}

