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
    def ls: Int
    def r: Point
    def evolve: World
  }

  def mkWorld(data: String): World

  sealed abstract class State(w: World, val steps: Int, ls: Int) {
    def score = 25 * ls - steps

    def status: String

    override def toString = "status = " + status + "\n" + w
  }

  case class Game(w: World, override val steps: Int, ls: Int) extends State(w, steps, ls) {
    def status = "in progress"
    def step(c: Command): State =
      c match {
        case Abort =>
          Aborted(w, steps + 1, ls) // steps + 1 or steps?
        case _ =>
          var w = this.w

          val nextR = w.r + c.dir
          val afterR = nextR + c.dir
          c match {
            case Up | Down | Left | Right if w(nextR).isPassable =>
              w = w.update(w.r, Empty)
              w = w.update(nextR, Robot)
            case Left | Right if w(nextR) == Rock && w(afterR) == Empty =>
              w = w.update(w.r, Empty)
              w = w.update(nextR, Robot)
              w = w.update(afterR, Rock)
            case _ => // do nothing
          }

          w = w.evolve

          val nextLs = ls + w.ls - this.w.ls
          this.w(nextR) match {
            case _ if w.r == Invalid =>
              Lost(w, steps + 1, nextLs)
            case Open =>
              Won(w, steps + 1, nextLs)
            case _ =>
              Game(w, steps + 1, nextLs)
          }
      }
    }

  case class Lost(w: World, override val steps: Int, ls: Int) extends State(w, steps, ls) {
    def status = "lost"
  }

  case class Aborted(w: World, override val steps: Int, ls: Int) extends State(w, steps, ls) {
    def status = "aborted"
    override def score = super.score + 25 * ls
  }

  case class Won(w: World, override val steps: Int, ls: Int) extends State(w, steps, ls) {
    def status = "won"
    override def score = super.score + 50 * ls
  }

  def mkGame(data: String): Game = Game(mkWorld(data), 0, 0)
}

trait WorldsImpl extends Worlds {
  case class World(data: List[List[Item]]) extends WorldApi {
    def apply(p: Point) = if (data.isDefinedAt(p.x) && data(p.x).isDefinedAt(p.y)) data(p.y)(p.x) else Wall

    def update(p: Point, item: Item) =
      World(data.zipWithIndex map {
        case (line, y) =>
          line.zipWithIndex map {
            case (_, x) if x == p.x && y == p.y => item
            case (item, _) => item
          }
      })

    def h = data.length
    def w = data(0).length

    def r: Point = {
      for (x <- 0 to w; y <- 0 to h if this(x, y) == Robot) return Point(x, y)
      Invalid
    }

    def ls = data.flatten.count(_ == Lambda)

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
        } else if (this(x,y) == Closed && ls == 0) {
          w = w.update((x, y), Open)
        }
      }
      w
    }

    override def toString = data.reverse.map(_.mkString).mkString("\n")
  }

  def mkWorld(data: String) = World(data.split("\n").map(_.map(c => Item.unapply(c).get).toArray.toList).toList.reverse)
}

object Validator extends App with Worlds with WorldsImpl {
  if (args.length != 2) {
    println("usage: Validator <map filename> <commands>")
    System.exit(255)
  }
  val map = scala.io.Source.fromFile(args(0)).getLines().mkString("\n")
  println(map)
  println()
  val commands = args(1) map (Command.unapply(_).get)
  def exit(g: State): Nothing = {
    println("%d of %d steps: %s".format(g.steps, commands.length, (commands take g.steps).mkString))
    println(g)
    sys.exit(0)
  }
  val result = commands.foldLeft(mkGame(map))((g, c) => {
    println("Step: %s".format(c))
    println(g.w)
    println()
    g.step(c) match {
      case g: Game => g
      case g: Lost => exit(g)
      case g: Aborted => exit(g)
      case g: Won => exit(g)
    }
  })
  exit(result)
}