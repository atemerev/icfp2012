package icfp
package emulator

trait Items {
  self: Emulator =>

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
}