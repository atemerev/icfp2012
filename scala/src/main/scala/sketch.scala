package icfp

object Prelude {

  case class Point(x: Int, y: Int) {
    def +(p: Point) = Point(x + p.x, y + p.y)
  }

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

    def ls: Int

    def r: Point

    def apply(point: Point): Item

    def update(point: Point, item: Item): World

    def h: Int

    def w: Int
  }

  def mkWorld(data: String): World

  case class Log(w: World, steps: Int, ls: Int) {
    def next(c: Command): Log = {
      var w = this.w
      val nextR = w.r + c.dir
      val afterR = nextR + c.dir

      c match {
        case Up | Down | Left | Right if w(nextR).isPassable =>
          w(w.r) = Empty
          w(nextR) = Robot
        case Left | Right if w(nextR) == Rock && w(afterR) == Empty =>
          w(w.r) = Empty
          w(nextR) = Robot
          w(afterR) = Rock
      }

      Log(w, steps + 1, ls + (if(this.w(nextR) == Lambda) 1 else 0))
    }
  }

  def mkLog(data: String): Log = ???


}

trait WorldsImpl extends Worlds {

  case class World(array: List[List[Item]]) extends WorldApi {

    def apply(p: Point) = if (array.isDefinedAt(p.x) && array(p.x).isDefinedAt(p.y)) array(p.x)(p.y) else Wall

    def h = array.length

    def w = array(0).length

    def r = ???

    def ls = ???

    def update(point: Point, item: Item) = ???
  }

  def mkWorld(data: String) = World(data.split("\n").map(_.map(c => Item.unapply(c).get).toArray.toList).toList)
}

object Sketch extends Worlds with WorldsImpl


