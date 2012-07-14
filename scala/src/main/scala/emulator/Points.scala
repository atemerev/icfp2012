package icfp
package emulator

trait Points {
  self: Emulator =>

  case class Point(x: Int, y: Int) {
    def +(p: Point) = Point(x + p.x, y + p.y)
    def distanceTo(p: Point) = if (isValid && p.isValid) math.abs(x - p.x) + math.abs(y - p.y) else Int.MaxValue
    def isValid = this != Invalid
  }

  val Invalid = Point(-1, -1)

  implicit def pointToTuple(p: Point): (Int, Int) = (p.x, p.y)
  implicit def tupleToPoint(t: (Int, Int)): Point = Point(t._1, t._2)
}