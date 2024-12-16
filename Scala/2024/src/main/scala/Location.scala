case class Location(x: Int, y: Int) {
  def translate(vector: (Int, Int), offset: Int): Location =
    Location(
      x + vector._1 * offset,
      y + vector._2 * offset
    )
}