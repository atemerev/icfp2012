package icfp

// (xb to vp) replacing an injected SUT with a singleton is bad
// but I cannot immediately see the consequences in this particular case
object Tests {
  import Main._

  def fmt(x: Any) = if (x.toString contains "\n") ("\n" + x + "\n") else (" <" + x + ">")
  def run = tests mapValues {
    test =>
    try {
      val p = test()
      if (p._1 == p._2) "ok" else "expected" + fmt(p._1) + ", actual" + fmt(p._2)
    } catch {
      case e:Exception => e.getMessage
    }
  }

  def lines(source: String) = (source split "\n" tail) toList
  val smallWorld = mkWorld(lines(
    """
#* *#
#* *#
#####"""
  ))

  val crossWorld = mkWorld(lines(
"""
        #L#######
        #*** \\ #
        #\\\ .. #
#########.##    ##########
#.......\ ..........*   .#
#*******\......#....#\\ .#
###\.\\\...**..#....... *#
#*****\\  .\\..##     #\.#
######### ....  ##########
        #       #
        ####*####
        #.......#
#########  \\\\*##########
#*\\  **#     *..*\ \\\\\#
#.\**\*** .....**.# \\##\#
#\R......     .\\.. \\\\\#
##########################"""
  ))

val deepWell = """
#R  #
#   #
#   #
#   #
#   #
#   #
#   #
###L#"""

  val dryWell = mkWorld(lines(deepWell))
  val slowFillingWell = mkWorld(lines(deepWell + "\n\nFlooding 5\nWaterproof 3"))
  val fastFillingWell = mkWorld(lines(deepWell + "\n\nFlooding 2\nWaterproof 3"))

  def newState(world: World) = new InProgress(world, 0, 0, 0){
    override def status: String = "I'm okay so far; you?"
  }

  val tests:Map[String, ()=>(Any, Any)] = Map(
    "varying width" -> (() => {
      val game = mkGame(crossWorld)
      (26, game.w)
    }),
  "wall at 0,0" -> (() => { (Wall, smallWorld((0, 0))) }),
  "wall at 4,4" -> (() => { (Wall, smallWorld((4, 4))) }),
  "wall at 2,3" -> (() => { (Wall, smallWorld((2, 3))) }),
  "Rock at 1,2" -> (() => { (Rock, smallWorld((1, 2))) }),
  "Void at 2,1" -> (() => { (Empty,smallWorld((2, 1))) }),

  "rocks fall" -> (() => {
    val world = mkWorld(lines(
      """
#* #
#* #
####"""
    ))
    val expected = mkWorld(lines(
      """
#  #
#**#
####"""
    ))
    (expected, world.evolve)
  }),

  "rocks merge" -> (() => {
    val expected = mkWorld(lines(
      """
#   #
#***#
#####"""
    ))
    (expected, smallWorld.evolve)
  }),
  "find lift" -> (() => {
    (Point(9,16), crossWorld.lift)
  }),
  "lambda closest to lift" -> (() => {
    (Point(9,14), crossWorld.lambdaClosestToLift)
  }),
  "no flooding in a dry well" -> (() => {
    (0, dryWell.flooding)
  }),
  "have flooding in a wet well" -> (() => {
    (5, slowFillingWell.flooding)
  }),
  "time to next flooding is good" -> (() => {
    val world = mkWorld(lines(deepWell + "\n\nFlooding 5\nWaterproof 3"), 8)
    (2, world.timeToNextFlood)
  }),
  "ttl under water in a dry well" -> (() => {
    val state = newState(dryWell)
    (Int.MaxValue, state.timeToLiveUnderWater)
  }),
  "ttl under water in a wet well" -> (() => {
    (3, newState(slowFillingWell).timeToLiveUnderWater)
  }),
  "able to get to lift in a dry well" -> (() => {
    (true, newState(dryWell).mayGetToLift)
  }),
  "able to get to lift in a slow-filling well" -> (() => {
    (true, newState(slowFillingWell).mayGetToLift)
  }),
  "unable to get to lift in a fast-filling well" -> (() => {
    (false, newState(fastFillingWell).mayGetToLift)
  })
  )
}