package icfp
package strategies

trait Pathfinder {
  self: emulator.Commands with emulator.Games with emulator.Items with emulator.Points with emulator.States with emulator.Worlds =>

  type Path = List[Command]
  type DistanceMap = Array[Array[Int]]

  def mkDistMap(w: World, dest: Point): DistanceMap = {
    val p: DistanceMap = Array.fill(w.w, w.h)(Int.MaxValue)
    p(dest.x)(dest.y) = 0
    var somethingChanged = true
    while (somethingChanged) {
      somethingChanged = false
      for (x <- 0 until w.w; y <- 0 until w.h if w(x, y) == Empty || w(x, y) == Earth || w(x, y) == Lambda || w(x, y) == Robot) {
        var v = p(x)(y) - 1
        if (x > 0) v = math.min(v, p(x - 1)(y))
        if (y > 0) v = math.min(v, p(x)(y - 1))
        if (x < w.w - 1) v = math.min(v, p(x + 1)(y))
        if (y < w.h - 1 && w(x, y + 1) != Rock) v = math.min(v, p(x)(y + 1))
        if (v + 1 != p(x)(y)) {
          p(x)(y) = v + 1
          somethingChanged = true
        }
      }
    }
    p
  }

  def findPath(start: Point, p: DistanceMap): Option[Path] = {
    var x = start.x
    var y = start.y
    val dist = p(x)(y)
    if (dist > p.length * p(0).length) None
    else {
      val path = Array.ofDim[Command](dist)
      for (i <- 0 to dist) {
        if (x > 0 && p(x - 1)(y) < dist - i) {
          path(i) = Left
          x = x - 1
        } else if (y > 0 && p(x)(y - 1) < dist - i) {
          path(i) = Down
          y = y - 1
        } else if (x < p.length - 1 && p(x + 1)(y) < dist - i) {
          path(i) = Right
          x = x + 1
        } else if (y < p(0).length - 1 && p(x)(y + 1) < dist - i) {
          path(i) = Up
          y = y + 1
        }
      }
      Some(path.toList)
    }
  }
}