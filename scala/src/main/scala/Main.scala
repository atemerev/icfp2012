package icfp

object Main extends App with DumbEmulator with Strategies with emulator.Cli {
  def loadLines(url: String): List[String] =
    // (xb) would also be great to automatically download samples from the server
    scala.io.Source.fromFile(url).getLines().toList

  def loadLinesStdin: List[String] = io.Source.fromInputStream(System.in).getLines().toList

  // (xb to at) todo. SIGINT handling

  val cmd = args(0)
  val stuff = args.tail
  cmd match {
    case "r" =>
      if (stuff.length != 2) { println("usage: r <url of map> <commands>"); sys.exit(-1) }
      val game = mkGame(mkWorld(loadLines(stuff(0))))
      val commands = mkCommands(stuff(1))
      runNonInteractive(game, commands)
    case "i" =>
      if (stuff.length != 1) { println("usage: i <url of map>"); sys.exit(-1) }
      val game = mkGame(mkWorld(loadLines(stuff(0))))
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
      if (stuff.length > 1) { println("usage: gen1 [map]"); sys.exit(-1) }
      val game = if (stuff.length == 0) mkGame(mkWorld(loadLinesStdin)) else mkGame(mkWorld(loadLines(stuff(0))))
      val trace = stuff.length == 1
      val commands = genetic1(game, trace)
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
  }
}
