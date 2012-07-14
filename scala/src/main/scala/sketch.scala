package icfp

import Prelude._

trait Games {
  self: RobotStates with Worlds =>

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

trait StuffWeRun extends Games with Worlds with WorldsImpl with RobotStates {
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
    val result = Tests(this).run
    val report: String = result.filter(p => "ok" != p._2) mkString "\n"
    println(if (report.isEmpty) "Passed" else report)
    System.exit(report.length)
  }
  def genetic1(args: Array[String]) {
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

}

object Main extends App with StuffWeRun {
  val cmd = args(0)
  val stuff = args.tail
  cmd match {
    case "v" => validate(stuff)
    case "i" => interpret(stuff)
    case "t" => tests(stuff)
    case "g" => genetic1(stuff)
  }
}
