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
  test("day 1 part 1 passes") {
    val actual = Day1_1().process(day1_input)
    val expected = Right(11)
    assertEquals(actual, expected)
  }

  test("day 1 part 2 passes") {
    val actual = Day1_2().process(day1_input)
    val expected = Right(31)
    assertEquals(actual, expected)
  }
}
