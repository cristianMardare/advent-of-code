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

  def checkDay2_1(
               description: String,
               in: List[Int],
               expected: Security
               ): Unit = {
    test("[day 2][part 1]" + description) {
      val actual = Day2_1().checkReports(in)

      assertEquals(actual, expected)
    }
  }

  checkDay2_1("All decreasing by 1 or 2", List(7, 6, 4, 2, 1), Safe)
  checkDay2_1("An increase of 5", List(1, 2, 7, 8, 9), Unsafe)
  checkDay2_1("A decrease of 4", List(9, 7, 6, 2, 1), Unsafe)
  checkDay2_1("Increasing, but one sequence decreasing", List(1, 3, 2, 4, 5), Unsafe)
  checkDay2_1("Neither an increase or decrease", List(8, 6, 4, 4, 1), Unsafe)
  checkDay2_1("All increasing by 1, 2 or 3", List(1, 3, 6, 7, 9), Safe)

  def checkDay2_2(
                   description: String,
                   in: List[Int],
                   expected: Security
                 ): Unit = {
    test("[day 2][part 2]" + description) {
      val actual = Day2_2().checkReport(in)

      assertEquals(actual, expected)
    }
  }

  checkDay2_2("Safe without removing any level.", List(7, 6, 4, 2, 1), Safe)
  checkDay2_2("Unsafe regardless of which level is removed.", List(1, 2, 7, 8, 9), Unsafe)
  checkDay2_2("Unsafe regardless of which level is removed.", List(9, 7, 6, 2, 1), Unsafe)
  checkDay2_2("Safe by removing the second level, 3.", List(1, 3, 2, 4, 5), Safe)
  checkDay2_2("Safe by removing the third level, 4.", List(8, 6, 4, 4, 1), Safe)
  checkDay2_2("Safe without removing any level.", List(1, 3, 6, 7, 9), Safe)
  checkDay2_2("Safe by removing the fourth level, 25.", List(29, 28, 27, 25, 26, 25, 22, 20), Safe)
  checkDay2_2("Safe by removing the second level, 10.", List(7, 10, 8, 10, 11), Safe)

}
