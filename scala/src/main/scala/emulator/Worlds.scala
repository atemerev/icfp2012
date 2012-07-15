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
    def collectedLambdas: Int
    def remainingLambdas: Int
    def remainingLambdaPositions: List[Point]
    def robot: Point
    def lift: Point
    def liftIsOpen: Boolean
    def water: Int
    def flooding: Int
    def waterproof: Int
    def razors: Int
    def heightOverWater = robot.y - water
    def isUnderwater: Boolean = heightOverWater < 0
    def timeToNextFlood: Int
    def lambdaClosestToLift: Point
    def evolve: World
    def isFinal: Boolean
    def finalCommand: Command
    def liftIsBlockedForever: Boolean
    def trampolines: Map[Trampoline, Point]
    def trampolinePositions: List[Point]
    def metadata: Map[String, String]
    def updateMetadata(metadata: Map[String, String]): World
  }

  def mkWorld(lines: List[String], age: Int = 0): World
}

trait DumbWorlds {
  self: Commands with Games with Items with Points with States with Worlds =>

  case class World(data: List[List[Item]], trampolines: Map[Trampoline, Point], override val metadata: Map[String, String], age: Int, collectedLambdas: Int, override val remainingLambdas: Int) extends WorldApi {
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

    def update(p: Point, item: Item) = {
      val lambdasCollectedNow = if (this(p.x, p.y) == Lambda) 1 else 0
      World(data.zipWithIndex map {
        case (line, y) =>
          line.zipWithIndex map {
            case (_, x) if x == p.x && y == p.y => item
            case (item, _) => item
          }
      }, trampolines, metadata, age, collectedLambdas + lambdasCollectedNow, remainingLambdas - lambdasCollectedNow)
    }

    def updateMetadata(metadata: Map[String, String]): World = World(data, trampolines, metadata, age, collectedLambdas, remainingLambdas)

    def h = data.length
    def w = data(0).length

    def water = metadata.getOrElse("Water", "0").toInt
    def water_=(value: Int): World = World(data, trampolines, metadata + ("Water" -> value.toString), age, collectedLambdas, remainingLambdas)
    def flooding = metadata.getOrElse("Flooding", "0").toInt
    def waterproof = metadata.getOrElse("Waterproof", "10").toInt
    def timeToNextFlood = if (flooding == 0) Int.MaxValue else (flooding - age % flooding)
    def razors = metadata.getOrElse("Razors", "0").toInt
    def growth = metadata.getOrElse("Growth", "25").toInt

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

    def liftIsOpen = {
      apply(lift) == Open
    }

    def blocked(x: Int, y: Int) = this(x, y) == Wall || this(x, y).isRock

    def liftIsBlockedForever = {
      lift.x == 0   && blocked(  1, lift.y) && (blocked(  1, lift.y-1) || blocked(  1, lift.y+1)) ||
      lift.x == w-1 && blocked(w-2, lift.y) && (blocked(w-2, lift.y-1) || blocked(w-2, lift.y+1)) ||
      lift.y == 0   && blocked(lift.x,   1) && (blocked(lift.x-1,   1) || blocked(lift.x+1,   1)) ||
      lift.y == h-1 && blocked(lift.x, h-2) && (blocked(lift.x-1, h-2) || blocked(lift.x+1, h-2))
    }

    def nearLift = robot.distanceTo(lift) < 2

    def isFinal = remainingLambdas == 0 && nearLift

    def finalCommand = if (!isFinal || robot == Invalid || !liftIsOpen) null else {
      if (lift == Point(robot.x, robot.y + 1)) Up else
      if (lift == Point(robot.x, robot.y - 1)) Down else
      if (lift == Point(robot.x - 1, robot.y)) Left else
      if (lift == Point(robot.x + 1, robot.y)) Right else ???
    }

    def distanceToLift(p: Point) = p.distanceTo(lift)

    def lambdaClosestToLift = tupleToPoint((lambdas map (p => (distanceToLift(p), p)) min)._2)

    private var robotAt: Point = Invalid

    def robot: Point = {
      if (robotAt == Invalid) for (p <- find(Robot)) robotAt = p
      robotAt
    }

    def remainingLambdaPositions = lambdas map { case (x, y) => Point(x, y) } toList

    def trampolinePositions = points filter (tup => this(tup).isTrampoline) map { case (x, y) => Point(x, y) } toList

    private def putRock(p: Point, item: Item): World = {
      var w = update(p, item match {
        case Rock(true) if this(p.x, p.y -1) != Empty => Lambda
        case _ => item
      })
      if (w(p.x, p.y - 1) == Robot) w = w.update((p.x, p.y - 1), Empty)
      w
    }

    private def rockDrops(x: Int, y: Int): World = {
      val rock = this(x, y)
      update((x, y), Empty).putRock((x, y - 1), rock)
    }

    private def rockRollsLeft(x: Int, y: Int): World = {
      val rock = this(x, y)
      update((x, y), Empty).putRock((x - 1, y - 1), rock)
    }

    private def rockRollsRight(x: Int, y: Int): World = {
      val rock = this(x, y)
      update((x, y), Empty).putRock((x + 1, y - 1), rock)
    }

    def evolve: World = {
      var w = this
      w = World(data, trampolines, metadata, age + 1, collectedLambdas, remainingLambdas)
      val growBeardNow = (growth != 0 && age != 0 && (age % growth == 0))
      val floodNow = flooding != 0 && age != 0 && (age % flooding == 0)
      for (x <- 0 to this.w; y <- 0 to this.h) {
        val here       = this(x,     y)
        val below      = this(x,     y - 1)
        val onLeft     = this(x - 1, y)
        val onRight    = this(x + 1, y)
        val belowLeft  = this(x - 1, y - 1)
        val belowRight = this(x + 1, y - 1)

        if (here.isRock && below == Empty) { // rule 1
          w = w.rockDrops(x, y)
        } // the next condition will not hold if the above condition held
        if (here.isRock && below.isRock && onRight == Empty && belowRight == Empty) { // rule 2
          w = rockRollsRight(x, y)
        }
        if (here.isRock && below.isRock && (onRight != Empty || belowRight != Empty) && onLeft == Empty && belowLeft == Empty) { // rule 3
          w = w.rockRollsLeft(x, y)
        }
        if (here.isRock && below == Lambda && onRight == Empty && belowRight == Empty) { // rule 4, rolling over lambda
          w = w.rockRollsRight(x, y)
        } else if (here == Closed && remainingLambdas == 0) { // rule 5
          w = w.update((x, y), Open)
        } else if (here == Beard && growBeardNow) {
          if (this(x + 0, y + 1) == Empty) w = w.update((x + 0, y + 1), Beard)
          if (this(x + 0, y - 1) == Empty) w = w.update((x + 0, y - 1), Beard)
          if (this(x + 1, y + 0) == Empty) w = w.update((x + 1, y + 0), Beard)
          if (this(x + 1, y + 1) == Empty) w = w.update((x + 1, y + 1), Beard)
          if (this(x + 1, y - 1) == Empty) w = w.update((x + 1, y - 1), Beard)
          if (this(x - 1, y + 0) == Empty) w = w.update((x - 1, y + 0), Beard)
          if (this(x - 1, y + 1) == Empty) w = w.update((x - 1, y + 1), Beard)
          if (this(x - 1, y - 1) == Empty) w = w.update((x - 1, y - 1), Beard)
        }
      }
      if (floodNow) w = w.water_=(water + 1)
      w
    }

    override def toString = data.reverse.map(_.mkString).mkString("\n") + (if (razors == 0) "" else " (%d razor%s)".format(razors, if (razors == 1) "" else "s"))
    override def hashCode = data.hashCode
    override def equals(o: Any) = o.isInstanceOf[World] && data == o.asInstanceOf[World].data

//    def cutTop(left: Int, right: Int) = {
//      // todo: fill the ceiling with wall blocks, to decrease the space for search
//      this
//    }
//
//    def tryCutTop: Option[World] = {
//      if (lift.y < h-2) {
//        for (left  <- (0 to w) find (x => this(x, h - 1) == Wall);
//             right <- (left to w) find (x => this(x, h - 1) != Wall)
//             if ((left to (right - 1)) forall (x => this(x, h-2) == Empty && this(x, h-3) == Empty))) {
//               return Some(cutTop(left, right - 1))
//             }
//      } else None
//    }
  }

  def mkWorld(lines0: List[String], age: Int = 0) = { // (xb to vp) why explicit age?
    val lines = lines0 filter (!_.startsWith(";;"))
    val mine: List[String] = lines dropWhile (_.isEmpty) takeWhile (!_.isEmpty)
    val trampolines = collection.mutable.Map[Char, Char]()
    var metadata: Map[String, String] = lines drop (mine.length + 1) takeWhile (!_.isEmpty) map (line => {
      if (line startsWith "Trampoline") {
        val Format = """^Trampoline (.) targets (.)$""".r
        val Format(name, destination) = line
        trampolines += (name(0) -> destination(0))
        ("", "")
      } else {
        val Format = """^(\w+)\s+(.+?)\s*$""".r
        val Format(key, value) = line
        (key, value)
      }
    }) toMap;
    val targets = collection.mutable.Map[Trampoline, Point]()
    val parsed = mine.zipWithIndex map { case (line, y) =>
      line.zipWithIndex map { case (c, x) =>
        if ('1' <= c && c <= '9') {
          val sources = trampolines collect { case (name, destination) if destination == c => Trampoline(name) }
          sources foreach (tramp => targets += tramp -> (x, y))
          Empty
        } else {
          try {
            Item.unapply(c).get
          } catch {
            case ex =>
              println("failed to parse: " + c)
              throw ex
          }
        }
      } toList
    } toList;
    for ((t, Point(x, y)) <- targets.toList) targets(t) = Point(x, parsed.length - y - 1)
    val width = parsed map (_.length) max
    val padded = parsed map (_.padTo(width, Empty))
    World(padded.reverse, targets.toMap, metadata, 0, 0, padded.flatten.collect{case Rock(true) => 1; case Lambda => 1}.length)
  }
}
