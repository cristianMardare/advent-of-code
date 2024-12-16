import Day6.{Downwards, Empty, Leftwards, Movement, Obstruction, Place, Rightwards, Upwards, Visited}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.control.Breaks._

/*
The Historians use their fancy device again, this time to whisk you all away to the North Pole prototype suit manufacturing lab... in the year 1518! It turns out that having direct access to history is very convenient for a group of historians.

You still have to be careful of time paradoxes, and so it will be important to avoid anyone from 1518 while The Historians search for the Chief. Unfortunately, a single guard is patrolling this part of the lab.

Maybe you can work out where the guard will go ahead of time so that The Historians can search safely?

You start by making a map (your puzzle input) of the situation. For example:

....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
The map shows the current position of the guard with ^ (to indicate the guard is currently facing up from the perspective of the map). Any obstructions - crates, desks, alchemical reactors, etc. - are shown as #.

Lab guards in 1518 follow a very strict patrol protocol which involves repeatedly following these steps:

If there is something directly in front of you, turn right 90 degrees.
Otherwise, take a step forward.
Following the above protocol, the guard moves up several times until she reaches an obstacle (in this case, a pile of failed suit prototypes):

....#.....
....^....#
..........
..#.......
.......#..
..........
.#........
........#.
#.........
......#...
Because there is now an obstacle in front of the guard, she turns right before continuing straight in her new facing direction:

....#.....
........>#
..........
..#.......
.......#..
..........
.#........
........#.
#.........
......#...
Reaching another obstacle (a spool of several very long polymers), she turns right again and continues downward:

....#.....
.........#
..........
..#.......
.......#..
..........
.#......v.
........#.
#.........
......#...
This process continues for a while, but the guard eventually leaves the mapped area (after walking past a tank of universal solvent):

....#.....
.........#
..........
..#.......
.......#..
..........
.#........
........#.
#.........
......#v..
By predicting the guard's route, you can determine which specific positions in the lab will be in the patrol path. Including the guard's starting position, the positions visited by the guard before leaving the area are marked with an X:

....#.....
....XXXXX#
....X...X.
..#.X...X.
..XXXXX#X.
..X.X.X.X.
.#XXXXXXX.
.XXXXXXX#.
#XXXXXXX..
......#X..
In this example, the guard will visit 41 distinct positions on your map.

Predict the path of the guard. How many distinct positions will the guard visit before leaving the mapped area?
 */
class Day6_1 extends Challenge[List[Char], Int] {

  override def getInputName: String = "day6.txt"

  override def parse(s: Source): Either[Exception, List[List[Char]]] = {
    try {
      val result =
        s.getLines()
          .map(s => s.toCharArray.toList)
          .toList
      Right(result)
    } catch {
      case e: Exception => Left(e)
    }
  }

  override def process(in: List[List[Char]]): Either[Exception, Int] = {
    try {

      val board = asBoard(in)
      val (start, direction) = getStartAndMovement(in).get
      val game = Game(board)

      game.run(start, direction) match {
        case OutOfBounds(finalBoard) =>
          val result = finalBoard.count {
            case Visited(_) => true
            case _ => false
          }
          Right(result)
        case InfiniteLoop(finalBoard) => Left(Exception("Cannot finish movement because of infinite loop"))
      }


    } catch {
      case e: Exception => Left(e)
    }
  }

  protected def asBoard(in: List[List[Char]]): Board[Place] = {
   val matrix = in.map(l => l.toArray).toArray
    val result = matrix.map(
      row => row.map(
        item =>
          if item == '#' then
            Obstruction
          else
            Empty
      )
    )

    Board(result)

  }

  protected def getStartAndMovement(in: List[List[Char]]): Option[(Location, Movement)] = {
    var result: Option[(Location, Movement)] = None

    breakable {
      for (i <- in.indices)
        for (j <- in.head.indices) {
          in(i)(j) match {
            case '>' => result = Some((Location(i, j), Rightwards))
            case 'v' => result = Some((Location(i, j), Downwards))
            case '<' => result = Some((Location(i, j), Leftwards))
            case '^' => result = Some((Location(i, j), Upwards))
            case _ => result = None
          }

          if (result.isDefined)
            break()
        }
    }

    result
  }
}

trait GameOutcome[T]
case class InfiniteLoop[T](finalBoard: Board[T]) extends GameOutcome[T]
case class OutOfBounds[T](finalBoard: Board[T]) extends GameOutcome[T]

class Game(b: Board[Place]) {
  def run(start: Location, direction: Movement): GameOutcome[Place] = {
    @tailrec
    def runRec(current: Location, dir: Movement, b: Board[Place]): GameOutcome[Place] =
      if (b.isInBounds(current)){
        b getAt current match {
          case Some(Visited(d)) if d == dir => InfiniteLoop(b) // on an already visited place, the last direction matches the current direction (meaning a loop will follow)
          case Some(place) =>
              if (place == Empty)
                b.replace(current, Visited(dir))
              val next = dir.next(current)
              if ((b getAt next).contains(Obstruction))
                runRec(current, dir.rotate, b)
              else
                runRec(next, dir, b)
        }

      }
      else
        OutOfBounds(b)

    runRec(start, direction, b)
  }
}

object Day6 {

  abstract class Movement {
    def next(from: Location): Location
    def rotate: Movement
  }

  case object Upwards extends Movement {
    override def next(from: Location): Location =
      Location(
        from.x - 1,
        from.y
      )

    override def rotate: Movement = Rightwards
  }

  case object Rightwards extends Movement {
    override def next(from: Location): Location =
      Location(
        from.x,
        from.y + 1
      )

    override def rotate: Movement = Downwards
  }

  case object Downwards extends Movement {
    override def next(from: Location): Location =
      Location(
        from.x + 1,
        from.y
      )

    override def rotate: Movement = Leftwards
  }

  case object Leftwards extends Movement {
    override def next(from: Location): Location =
      Location(
        from.x,
        from.y - 1
      )

    override def rotate: Movement = Upwards
  }

  trait Place
  case object Obstruction extends Place
  case class Visited(direction: Movement) extends Place
  case object Empty extends Place{
    def toVisited(dir: Movement): Visited =
      Visited(dir)
  }
}
