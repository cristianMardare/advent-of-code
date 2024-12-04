class BulkInputTests extends munit.FunSuite {
  val day2_input: List[List[Int]] = List(
    List(48, 46, 47, 49, 51, 54, 56),
    List(1, 1, 2, 3, 4, 5),
    List(1, 2, 3, 4, 5, 5),
    List(5, 1, 2, 3, 4, 5),
    List(1, 4, 3, 2, 1),
    List(1, 6, 7, 8, 9),
    List(1, 2, 3, 4, 3),
    List(9, 8, 7, 6, 7),
    List(7, 10, 8, 10, 11),
    List(29, 28, 27, 25, 26, 25, 22, 20)
  )

  test("[day 1][part 2]All Safe") {
    val actual = Day2_2().process(day2_input)
    val expected = Right(10)
    assertEquals(actual, expected)
  }
}
