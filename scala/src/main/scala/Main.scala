package icfp

object Main extends App with DumbEmulator with Strategies with emulator.Cli {
  def loadLines(url: String): List[String] =
    // (xb) would also be great to automatically download samples from the server
    scala.io.Source.fromFile(url).getLines().toList

  def loadLinesStdin: List[String] = io.Source.fromInputStream(System.in).getLines().toList

  // (xb to at) todo. SIGINT handling

  val cmd = args(0)
  val stuff = args.tail
  def readGame: State = {
    if (stuff.length == 0) mkGame(mkWorld(loadLinesStdin)) else
                           mkGame(mkWorld(loadLines(stuff(0))))
  }

  cmd match {
    case "r" =>
      if (stuff.length != 2) { println("usage: r <url of map> <commands>"); sys.exit(-1) }
      val game = readGame
      val commands = mkCommands(stuff(1))
      runNonInteractive(game, commands)
    case "i" =>
      if (stuff.length != 1) { println("usage: i <url of map>"); sys.exit(-1) }
      val game = readGame
      val commands = runInteractive(game)
      println(commands.mkString)
    case "t" =>
      // (xb to at) how to integrate with specs or whatever?
      if (stuff.length != 0) { println("usage: t"); sys.exit(-1) }
      val result = Tests.run
      val report: String = result.filter(p => "ok" != p._2) mkString "\n"
      println(if (report.isEmpty) "Passed" else report)
      sys.exit(report.length)
    case "gen1" =>
      if (stuff.length != 1 && stuff.length != 2) { println("usage: gen1 <url of map> [<trace>]"); sys.exit(-1) }
      val game = readGame
      val trace = stuff.length == 2
      Trace.isEnabled = true
      val commands = genetic1(game, 150, trace)
      println(commands.mkString)
    case "ast" => // renamed from a* to be consistent with sbt
      if (stuff.length != 1 && stuff.length != 2) { println("Usage: ast <url of map> [<trace>]"); sys.exit(-1) }
      val game = readGame
      val trace = stuff.length == 2
      Trace.isEnabled = true
      val commands = search(game, 150, trace)
      println(commands.mkString)
    case "chess" =>
      if (stuff.length != 1 && stuff.length != 2) { println("Usage: chess <url of map> [<trace>]"); sys.exit(-1) }
      val game = readGame
      val trace = stuff.length == 2
      Trace.isEnabled = true
      val commands = chess(game, 150, trace)
      println(commands.mkString)
    case "p" =>
      val game = mkGame(mkWorld(
        """
        #L#######
        #... \\ #
        #\\\ .. #
#########.##    ##########
#.......\ ..........*   .#
#*******\......#....#\\ .#
###\.\\\...**..#....... *#
#*****\\  .\\..##     #\.#
######### ....  ##########
        #       #
        #### ####
        #.......#
#########  \\\\*##########
#*\\  **#     *..*\ \\\\\#
#.\**\*** .....**.# \\##\#
#\R......     .\\.. \\\\\#
##########################""".split("\n").toList))

      val start = game.w.robot
      val end = game.w.lift
      println(findPath(start, mkDistMap(game.w, end)).mkString)
    case "tourney" =>
      if (stuff.length != 1 && stuff.length != 2) { println("Usage: tourney <algo> [<timeout in seconds (per map)>]"); sys.exit(-1) }
      val algo = stuff(0)
      val timeout = if (stuff.length == 1) 10 else stuff(1).toInt
      println("algo is " + algo)
      val data = System.getProperty("user.dir") + "/../data"
      val tests = new java.io.File(data).listFiles.toList.sorted
      tests foreach { test =>
        print(test.getName + "... ")
        val skip = !(test.getName.startsWith("0") || test.getName.startsWith("1") || test.getName.startsWith("spec"))
        // val skip = !test.getName.startsWith("spec")
        if (skip) println("skipped")
        else {
          val start = System.currentTimeMillis()
          try {
            val lines = loadLines(test.getCanonicalPath)
            val hiscore = if (lines(0).startsWith(";;")) lines(0).substring(2).trim else "???"
            val game = mkGame(mkWorld(lines))
            Trace.isEnabled = false
            val commands = algo match {
              case "gen1" => genetic1(game, timeout, false)
              case "ast" => search(game, timeout, false)
              case "chess" => chess(game, timeout, false)
            }
            val finalState = playGame(game, commands)
            print(finalState.score + " / " + hiscore)
          } catch {
            case _ => print("*")
          }
          val end = System.currentTimeMillis()
          println(" [" + (end - start) / 1000 + " s]")
        }
      }
  }
}
