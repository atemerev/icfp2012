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

import Prelude._

sealed trait Item {
  def isPassable: Boolean
}

object Item {
  def unapply(c: Char): Option[Item] = c match {
    case '*' => Some(Rock)
    case '.' => Some(Earth)
    case ' ' => Some(Empty)
    case '\\' => Some(Lambda)
    case '#' => Some(Wall)
    case 'R' => Some(Robot)
    case 'L' => Some(Closed)
    case 'O' => Some(Open)
    case _ => None
  }
}

case object Rock extends Item {
  override def toString = "*"
  def isPassable = false
}

case object Earth extends Item {
  override def toString = "."
  def isPassable = true
}

case object Empty extends Item {
  override def toString = " "
  def isPassable = true
}

case object Lambda extends Item {
  override def toString = "\\"
  def isPassable = true
}

case object Wall extends Item {
  override def toString = "#"
  def isPassable = false
}

case object Robot extends Item {
  override def toString = "R"
  def isPassable = false
}

case object Closed extends Item {
  override def toString = "L"
  def isPassable = false
}

case object Open extends Item {
  override def toString = "O"
  def isPassable = false
}

sealed trait Command {
  def dir: Point
}

object Command {
  def unapply(c: Char): Option[Command] = c match {
    case 'L' => Some(Left)
    case 'R' => Some(Right)
    case 'U' => Some(Up)
    case 'D' => Some(Down)
    case 'W' => Some(Wait)
    case 'A' => Some(Abort)
    case _ => None
  }
}

case object Left extends Command {
  def dir = (-1, 0)
  override def toString = "L"
}

case object Right extends Command {
  def dir = (1, 0)
  override def toString = "R"
}

case object Up extends Command {
  def dir = (0, 1)
  override def toString = "U"
}

case object Down extends Command {
  def dir = (0, -1)
  override def toString = "D"
}

case object Wait extends Command {
  def dir = (0, 0)
  override def toString = "W"
}

case object Abort extends Command {
  def dir = (0, 0)
  override def toString = "A"
}

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

trait Games {
  self: Worlds =>

  sealed abstract class State(val w: World, val steps: Int, collectedLambdas: Int) {
    def score = 25 * collectedLambdas - steps
    def status: String
    override def toString = "status = " + status + "\n" + w
  }

  case class Game(override val w: World, override val steps: Int, collectedLambdas: Int, stepsUnderwater: Int) extends State(w, steps, collectedLambdas) {
    def status = "in progress"
    def step(c: Command): State =
      c match {
        case Abort =>
          Aborted(w, steps + 1, collectedLambdas) // steps + 1 or steps?
        case _ =>
          var w = this.w

          val nextR = w.robot + c.dir
          val afterR = nextR + c.dir
          c match {
            case Up | Down | Left | Right if w(nextR).isPassable =>
              w = w.update(w.robot, Empty)
              w = w.update(nextR, Robot)
            case Left | Right if w(nextR) == Rock && w(afterR) == Empty =>
              w = w.update(w.robot, Empty)
              w = w.update(nextR, Robot)
              w = w.update(afterR, Rock)
            case _ => // do nothing
          }

          val stepsUnderwater1 = if (this.w.isUnderwater) this.stepsUnderwater + 1 else 0
          if (stepsUnderwater >= this.w.waterproof) w = w.update(w.robot, Empty)

          w = w.evolve

          val nextLambdas = collectedLambdas + this.w.remainingLambdas - w.remainingLambdas
          this.w(nextR) match {
            case _ if w.robot == Invalid =>
              Lost(w, steps + 1, nextLambdas)
            case Open =>
              Won(w, steps + 1, nextLambdas)
            case _ =>
              Game(w, steps + 1, nextLambdas, stepsUnderwater1)
          }
      }
    }

  case class Lost(override val w: World, override val steps: Int, collectedLambdas: Int) extends State(w, steps, collectedLambdas) {
    def status = "lost"
  }

  case class Aborted(override val w: World, override val steps: Int, collectedLambdas: Int) extends State(w, steps, collectedLambdas) {
    def status = "aborted"
    override def score = super.score + 25 * collectedLambdas
  }

  case class Won(override val w: World, override val steps: Int, collectedLambdas: Int) extends State(w, steps, collectedLambdas) {
    def status = "won"
    override def score = super.score + 50 * collectedLambdas
  }

  def mkGame(world: World): Game = Game(world, 0, 0, 0)

  def playGame(game: State, commands: Traversable[Command]): State =
    commands.foldLeft(game)((s, c) =>
      s match {
        case g: Game =>
          // println("Step: %s".format(c))
          // println(g.w)
          // println()
          g.step(c)
        case _ =>
          s
      }
    )
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

trait StuffWeRun extends Games with Worlds with WorldsImpl {
  def validate(args: Seq[String]) {
    if (args.length != 2) {
      println("usage: Validator <map filename> <commands>")
      System.exit(255)
    }
    val lines = scala.io.Source.fromFile(args(0)).getLines().toList
    println(lines mkString "\n")
    val commands = args(1) map (Command.unapply(_).get)
    val result = playGame(mkGame(mkWorld(lines)), commands)
    println("%d of %d steps: %s".format(result.steps, commands.length, (commands take result.steps).mkString))
    println(result)
  }

  def interpret(args: Seq[String]) {
    if (args.length != 1) {
      println("usage: Interpreter <map filename>")
      System.exit(255)
    }

    val lines = scala.io.Source.fromFile(args(0)).getLines().toList
    var game: Game = mkGame(mkWorld(lines))
    val moves = scala.collection.mutable.ListBuffer[Command]()

    while(true) {
      render(game)
      val c = jline.Terminal.getTerminal.readVirtualKey(System.in)
      val nextCommand = c match {
        case 'w' => Up
        case 's' => Down
        case 'a' => Left
        case 'd' => Right
        case ' ' => Abort
        case _ => Wait
      }
      val nextState = game.step(nextCommand)
      moves += nextCommand
      nextState match {
        case g: Game => game = g
        case _ =>
          render(nextState)
          println("Moves: " + moves.mkString + "\n")
          System.exit(0)
      }
    }
  }

  def render(state: State) {
    val cr = new jline.ConsoleReader()
    cr.clearScreen()
    println(state.w)
    println("Score: %d (%s)".format(state.score, state.status))
  }

  def tests(args: Seq[String]) {
    val result = Tests.tests mapValues { _(this) }
    println(result)
    System.exit(result.values.count(false ==))
  }
}

object Validator extends App with StuffWeRun {
  validate(args)
}

object Interpreter extends App with StuffWeRun {
  interpret(args)
}

object Main extends App with StuffWeRun {
  val cmd = args(0)
  val stuff = args.tail
  cmd match {
    case "v" => validate(stuff)
    case "i" => interpret(stuff)
    case "t" => tests(stuff)
  }
}

object Genetic1 extends App with Games with Worlds with WorldsImpl {
  if (args.length != 1) { println("usage: Genetic1 <map filename>"); System.exit(255) }
  val lines = scala.io.Source.fromFile(args(0)).getLines().toList
  var game: Game = mkGame(mkWorld(lines))

  def initialSize = 100
  def maxLength = game.w.h * game.w.w / 5

  def seed = 42
  def rng = new scala.util.Random(seed)
  def nextRandom: Int = rng.nextInt
  def pickRandom[T](xs: T*): T = xs.toSeq(rng.nextInt(xs.toSeq.length))

  type TaggedSeq = List[(Point, Command)]
  def TaggedSeq(xs: (Point, Command)*) = List(xs: _*)
  def eval(s: TaggedSeq): Int = playGame(game, s map (_._2)).score
  implicit object ord extends Ordering[TaggedSeq] { def compare(s1: TaggedSeq, s2: TaggedSeq) = -(eval(s1).compare(eval(s2))) }
  import scala.collection.immutable.SortedSet
  type Population = SortedSet[TaggedSeq]
  def Population(xs: Traversable[TaggedSeq]) = SortedSet[TaggedSeq](xs.toSeq: _*)

  def mkPopulation(count: Int = initialSize): Population = Population(gen take count toList)

  def gen: Iterator[TaggedSeq] = {
    def genOne: TaggedSeq = {
      var curPos = game.w.robot
      var moves = TaggedSeq()
      var attempts = -1
      def nextMove: (Point, Command) = {
        attempts += 1
        if (attempts >= maxLength) (curPos, Abort)
        else {
          val wannabe = pickRandom(Right, Left, Down, Up)
          val failed = moves find (_._1 == (curPos + wannabe.dir)) isDefined;
          if (failed) nextMove else { curPos = curPos + wannabe.dir; (curPos, wannabe) }
        }
      }
      val seq = Stream.empty.map((_: Nothing) => nextMove).takeWhile(x => x._2 != Abort)
      seq.toList :+ (Invalid, Abort)
    }
    Stream.empty.map((_: Nothing) => genOne).iterator
  }

  def evolve(p: Population): Population = {
    val allHybrids = for (a <- p; b <- p if (a.map(_._1).toSet intersect b.map(_._1).toSet).nonEmpty) yield crossover(a, b)
    val hybrids = allHybrids take p.size/5
    ((p ++ hybrids) take 4*p.size/5) ++ mkPopulation(p.size/5)
  }

  def crossover(s1: TaggedSeq, s2: TaggedSeq): TaggedSeq = {
    val ixn = pickRandom(s1.map(_._1) intersect s2.map(_._1): _*)
    val i1 = s1 indexOf ixn
    val i2 = s2 indexOf ixn
    (s1 take i1) ++ (s2 drop i2)
  }

  var p = mkPopulation()
  for (i <- 0 to 100) p = evolve(p)
  println(p.head map (_._2) mkString)
  println(eval(p.head))
}
