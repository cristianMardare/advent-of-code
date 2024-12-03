// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class SmallInputTests extends munit.FunSuite {
  val day1_input: List[(Int, Int)] = List(
    (3,4),
    (4,3),
    (2,5),
    (1,3),
    (3,9),
    (3,3)
  )
  test("[day 1][part 1]OK") {
    val actual = Day1_1().process(day1_input)
    val expected = Right(11)
    assertEquals(actual, expected)
  }

  test("[day 1][part 2]OK") {
    val actual = Day1_2().process(day1_input)
    val expected = Right(31)
    assertEquals(actual, expected)
  }

  def checkDay2(
               description: String,
               in: List[Int],
               expected: Security
               ) = {
    test("[day 2][part 1]" + description) {
      val actual = Day2_1().checkReports(in)

      assertEquals(actual, expected)
    }
  }

  checkDay2("All decreasing by 1 or 2", List(7, 6, 4, 2, 1), Safe)
  checkDay2("An increase of 5", List(1, 2, 7, 8, 9), Unsafe)
  checkDay2("A decrease of 4", List(9, 7, 6, 2, 1), Unsafe)
  checkDay2("Increasing, but one sequence decreasing", List(1, 3, 2, 4, 5), Unsafe)
  checkDay2("Neither an increase or decrease", List(8, 6, 4, 4, 1), Unsafe)
  checkDay2("All increasing by 1, 2 or 3", List(1, 3, 6, 7, 9), Safe)

}
