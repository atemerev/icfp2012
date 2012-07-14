package icfp

object Prelude {
  case class Point(x: Int, y: Int) {
    def +(p: Point) = Point(x + p.x, y + p.y)
  }

  val Invalid = Point(-1, -1)

  def ??? = throw new RuntimeException

  implicit def pointToTuple(p: Point): (Int, Int) = (p.x, p.y)
  implicit def tupleToPoint(t: (Int, Int)): Point = Point(t._1, t._2)
}

