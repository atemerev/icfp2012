package icfp

import icfp.Prelude.Point

case class Tests(w: StuffWeRun) {

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
  val smallWorld = w.mkWorld(lines(
    """
#* *#
#* *#
#####"""
  ))

  val crossWorld = w.mkWorld(lines(
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

  val tests:Map[String, ()=>(Any, Any)] = Map(
    "varying width" -> (() => {
      val game = w.mkGame(crossWorld)
      (26, game.w.w)
    }),
  "wall at 0,0" -> (() => { (Wall, smallWorld((0, 0))) }),
  "wall at 4,4" -> (() => { (Wall, smallWorld((4, 4))) }),
  "wall at 2,3" -> (() => { (Wall, smallWorld((2, 3))) }),
  "Rock at 1,2" -> (() => { (Rock, smallWorld((1, 2))) }),
  "Void at 2,1" -> (() => { (Empty,smallWorld((2, 1))) }),

  "rocks fall" -> (() => {
    val world = w.mkWorld(lines(
      """
#* #
#* #
####"""
    ))
    val expected = w.mkWorld(lines(
      """
#  #
#**#
####"""
    ))
    (expected, world.evolve)
  }),

  "rocks merge" -> (() => {
    val expected = w.mkWorld(lines(
      """
#   #
#***#
#####"""
    ))
    (expected, smallWorld.evolve)
  }),
  "find lift" -> (() => {
    (Point(9,16), crossWorld.whereLift)
  }),
    "lambda closest to lift" -> (() => {
      (Point(9,14), crossWorld.lambdaClosestToLift)
    })
  )
}
