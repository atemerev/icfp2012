package icfp
package emulator

trait Items {
  self: Commands with Games with Items with Points with States with Worlds =>

  sealed trait Item {
    def isPassable: Boolean
    def isRock: Boolean
    def isTrampoline: Boolean
  }

  object Item {
    def unapply(c: Char): Option[Item] = c match {
      case '*' => Some(Rock(nut = false)) // at, pls take a look; I believe it was a bug
      case '.' => Some(Earth)
      case ' ' => Some(Empty)
      case '\\' => Some(Lambda)
      case '#' => Some(Wall)
      case 'R' => Some(Robot)
      case 'L' => Some(Closed)
      case 'O' => Some(Open)
      case '@' => Some(Rock(nut = true))
      case 'A' => Some(Trampoline(name = c))
      case 'B' => Some(Trampoline(name = c))
      case 'C' => Some(Trampoline(name = c))
      case 'D' => Some(Trampoline(name = c))
      case 'E' => Some(Trampoline(name = c))
      case 'F' => Some(Trampoline(name = c))
      case 'G' => Some(Trampoline(name = c))
      case 'H' => Some(Trampoline(name = c))
      case 'I' => Some(Trampoline(name = c))
      case 'W' => Some(Beard)
      case '!' => Some(Razor)
      case _ => None
    }
  }

  case class Rock(nut: Boolean) extends Item {
    override def toString = if (nut) "@" else "*"
    def isPassable = false
    def isRock = true
    def isTrampoline = false
  }

  case object Earth extends Item {
    override def toString = "."
    def isPassable = true
    def isRock = false
    def isTrampoline = false
  }

  case object Empty extends Item {
    override def toString = " "
    def isPassable = true
    def isRock = false
    def isTrampoline = false
  }

  case object Lambda extends Item {
    override def toString = "\\"
    def isPassable = true
    def isRock = false
    def isTrampoline = false
  }

  case object Wall extends Item {
    override def toString = "#"
    def isPassable = false
    def isRock = false
    def isTrampoline = false
  }

  case object Robot extends Item {
    override def toString = "R"
    def isPassable = false
    def isRock = false
    def isTrampoline = false
  }

  case object Closed extends Item {
    override def toString = "L"
    def isPassable = false
    def isRock = false
    def isTrampoline = false
  }

  case object Open extends Item {
    override def toString = "O"
    def isPassable = false
    def isRock = false
    def isTrampoline = false
  }

  case class Trampoline(name: Char) extends Item {
    override def toString = (name + "").toString
    def isPassable = true
    def isRock = false
    def isTrampoline = true
  }

  case object Beard extends Item {
    override def toString = "W"
    def isPassable = false
    def isRock = false
    def isTrampoline = false
  }

  case object Razor extends Item {
    override def toString = "!"
    def isPassable = true
    def isRock = false
    def isTrampoline = false
  }
}