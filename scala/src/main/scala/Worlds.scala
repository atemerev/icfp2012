package icfp

import icfp.Prelude._

trait Worlds {
  type World <: WorldApi

  trait WorldApi {
    def apply(point: Point): Item
    def update(point: Point, item: Item): World
    def h: Int
    def w: Int
    def remainingLambdas: Int
    def robot: Point
    def water: Int
    def flooding: Int
    def waterproof: Int
    def isUnderwater: Boolean = water >= robot.y
    def evolve: World
  }

  def mkWorld(lines: List[String]): World
}

trait WorldsImpl extends Worlds {
  case class World(data: List[List[Item]], metadata: Map[String, String], age: Int) extends WorldApi {
    def apply(p: Point) = if (data.isDefinedAt(p.x) && data(p.x).isDefinedAt(p.y)) data(p.y)(p.x) else Wall

    def update(p: Point, item: Item) =
      World(data.zipWithIndex map {
        case (line, y) =>
          line.zipWithIndex map {
            case (_, x) if x == p.x && y == p.y => item
            case (item, _) => item
          }
      }, metadata, age)

    def h = data.length
    def w = data(0).length

    def water = metadata.getOrElse("Water", "0").toInt
    def water_=(value: Int): World = World(data, metadata + ("Water" -> value.toString), age)
    def flooding = metadata.getOrElse("Flooding", "0").toInt
    def waterproof = metadata.getOrElse("Waterproof", "10").toInt

    def robot: Point = {
      for (x <- 0 to w; y <- 0 to h if this(x, y) == Robot) return Point(x, y)
      Invalid
    }

    def remainingLambdas = data.flatten.count(_ == Lambda)

    private def putRock(p: Point): World = {
      var w = update(p, Rock)
      if (w(p.x, p.y - 1) == Robot) w = w.update((p.x, p.y - 1), Empty)
      w
    }

    def evolve: World = {
      var w = this

      for (x <- 0 to this.w; y <- 0 to this.h) {
        if (this(x, y) == Rock && this(x, y - 1) == Empty) {
          w = w.update((x, y), Empty)
          w = w.putRock(x, y - 1)
        } else if (this(x,y) == Rock && this(x, y - 1) == Rock && this(x + 1, y) == Empty && this(x + 1, y - 1) == Empty) {
          w = w.update((x, y), Empty)
          w = w.putRock(x + 1, y - 1)
        } else if (this(x,y) == Rock && this(x, y - 1) == Rock &&
          (this(x + 1, y) != Empty || (this(x + 1, y - 1) != Empty && this(x - 1, y) == Empty && this(x - 1, y - 1) == Empty))) {
          w = w.update((x, y), Empty)
          w = w.putRock(x - 1, y - 1)
        } else if (this(x,y) == Rock && this(x, y - 1) == Lambda && this(x + 1, y) == Empty && this(x + 1, y - 1) == Empty) {
          w = w.update((x, y), Empty)
          w = w.putRock(x + 1, y - 1)
        } else if (this(x,y) == Closed && remainingLambdas == 0) {
          w = w.update((x, y), Open)
        }
      }
      if (flooding != 0 && age != 0 && (age % flooding == 0)) w = w.water_=(water + 1)
      w
    }

    override def toString = data.reverse.map(_.mkString).mkString("\n")
  }

  val Format = """(\w+)\s+(.*)""".r

  def mkWorld(lines: List[String]) = {
    val map = lines takeWhile (!_.isEmpty)
    val metadata = lines drop (map.length + 1) map (line => {
      val Format(key, value) = line
      (key, value)
    }) toMap
    val parsed = map map (_ map (c => Item.unapply(c).get) toList) toList
    val width = parsed map (_.length) max
    val padded = parsed map (_.padTo(width, Empty))
    World(padded.reverse, metadata, 0)
  }
}
