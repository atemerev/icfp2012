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
import collection.immutable

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

  sealed abstract class State(steps: Int, ls: Int) {
    def score = 25 * ls - steps
  }

  case class Game(w: World, steps: Int, ls: Int) extends State(steps, ls) {
    def step(c: Command): State = {

      c match {
        case Abort =>
          Abort(steps + 1, ls) // steps + 1 or steps?
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
          }

          w = w.evolve

          val nextLs = ls + w.ls - this.w.ls
          this.w(nextR) match {
            case _ if w.r == Invalid =>
              Lost(steps + 1, nextLs)
            case Open =>
              Won(steps + 1, nextLs)
            case _ =>
              Game(w, steps + 1, nextLs)
          }
      }
    }
  }

  case class Lost(steps: Int, ls: Int) extends State(steps, ls)

  case class Abort(steps: Int, ls: Int) extends State(steps, ls) {
    def score = super.score + 25 * ls
  }

  case class Won(steps: Int, ls: Int) extends State(steps, ls) {
    def score = super.score + 50 * ls
  }

  def mkGame(data: String): Game = Game(mkWorld(data), 0, 0)
}

trait WorldsImpl extends Worlds {

  case class World(data: List[List[Item]]) extends WorldApi {

    def apply(p: Point) = if (data.isDefinedAt(p.x) && data(p.x).isDefinedAt(p.y)) data(p.x)(p.y) else Wall

    def update(point: Point, item: Item) = ???

    def h = data.length

    def w = data(0).length

    def r = ???

    def ls = ???

    def evolve = {
      ???
    }
  }

  def mkWorld(data: String) = World(data.split("\n").map(_.map(c => Item.unapply(c).get).toArray.toList).toList)
}

object Sketch extends Worlds with WorldsImpl


