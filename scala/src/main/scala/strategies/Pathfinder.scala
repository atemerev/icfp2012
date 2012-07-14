package icfp
package strategies

trait Pathfinder {
  self: emulator.Commands with emulator.Games with emulator.Items with emulator.Points with emulator.States with emulator.Worlds =>

  type Path = List[Command]
  type DistanceMap = Array[Array[Int]]

  def mkDistMap(w: World, dest: Point): DistanceMap = {
      val p: DistanceMap = Array.fill(w.h, w.w)(Int.MaxValue)
      p(dest.x)(dest.y) = 0
      for(i <- 0 to math.max(w.w, w.h)) {
        for (x <- 0 to w.h; y <- 0 to w.w if w(x,y) == Empty || w(x,y) == Earth || w(x,y) == Lambda ) {
          var v = p(x)(y) - 1
          if (x > 0) v = math.min(v, p(x - 1)(y))
          if (y > 0) v = math.min(v, p(x)(y - 1))
          if (x < w.w - 1) = math.min(v, p(x + 1)(y))
          if (y < w.h - 1 && p(x)(y + 1) + 1 < p(x)(y) && w(x, y) != Rock) v = math.min(v, p(x)(y + 1))
          p(x)(y) = v
        }
      }
      p
    }

  def findPath(start: Point, p: DistanceMap): Path = {
    var x = start.x
    var y = start.y
    val dist = p(x)(y)
    val path = Array.ofDim[Command](dist)
    for(i <- 0 to dist) {
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
    path.toList
  }
}
