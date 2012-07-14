package icfp
package emulator

trait Worlds {
  self: Commands with Games with Items with Points with States with Worlds =>

  type World <: WorldApi

  // (vp) curious, so we count from 0? how about water? Have to check it
  // (xb) VERY IMPORTANT!! need to get to it later

  trait WorldApi {
    def apply(point: Point): Item
    def moveTo(point: Point): World = update(point, Robot)
    def update(point: Point, item: Item): World
    def h: Int
    def w: Int
    def remainingLambdas: Int
    def robot: Point
    def lift: Point
    def water: Int
    def flooding: Int
    def waterproof: Int
    def heightOverWater = robot.y - water
    def isUnderwater: Boolean = heightOverWater < 0
    def timeToNextFlood: Int
    def lambdaClosestToLift: Point
    def evolve: World
  }

  def mkWorld(lines: List[String], age: Int = 0): World
}

trait DumbWorlds {
  self: Commands with Games with Items with Points with States with Worlds =>

  case class World(data: List[List[Item]], metadata: Map[String, String], age: Int) extends WorldApi {
    def apply(p: Point) = try {
      if (data.isDefinedAt(p.y) &&
          data(p.y).isDefinedAt(p.x))
      {
        data(p.y)(p.x)
      } else Wall
    } catch {case e: Exception => throw new IllegalArgumentException("fuck!@" + p + " in\n" + this, e)
    }

    // (xb to vp): never used?
    override def moveTo(point: Point): World = {
      val newWorld = update(point, Robot)
      newWorld.robotAt = point
      newWorld.liftAt = liftAt
      newWorld
    }

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
    def timeToNextFlood = if (flooding == 0) Int.MaxValue else (flooding - age % flooding)

    def isA(item: Item)(p: (Int, Int)) = this(p) == item
    def oneOf(items: Item*)(p: (Int, Int)) = items exists (this(p)==_)
    def points = for (x <- 0 to w; y <- 0 to h) yield (x, y)
    def lambdas = points filter (isA(Lambda))
    def find(what: Item): Option[(Int, Int)] = points find (isA(what))

    // make sense to pass it between generations, together with meta, in an additional props structure
    private var liftAt: Point = Invalid
    def lift = {
      if (liftAt == Invalid) for (p <- points find(oneOf(Open, Closed))) liftAt = p
      liftAt
    }

    def distanceToLift(p: Point) = p.distanceTo(lift)

    def lambdaClosestToLift = tupleToPoint((lambdas map (p => (distanceToLift(p), p)) min)._2)

    private var robotAt: Point = Invalid

    def robot: Point = {
      if (robotAt == Invalid) for (p <- find(Robot)) robotAt = p
      robotAt
    }

    def remainingLambdas = lambdas size

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

  def mkWorld(lines: List[String], age: Int = 0) = { // (xb to vp) why explicit age?
    val mine: List[String] = lines takeWhile (!_.isEmpty)
    val metadata: Map[String, String] = lines drop (mine.length + 1) takeWhile (!_.isEmpty) map (line => {
      val Format = """^(\w+)\s+(.+?)\s*$""".r
      val Format(key, value) = line
      (key, value)
    }) toMap
    val parsed = mine map (_ map (c => Item.unapply(c).get) toList) toList
    val width = parsed map (_.length) max
    val padded = parsed map (_.padTo(width, Empty))
    World(padded.reverse, metadata, 0)
  }
}
