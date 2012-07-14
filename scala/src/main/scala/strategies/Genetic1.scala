package icfp
package strategies

trait Genetic1 {
  self: emulator.Commands with emulator.Games with emulator.Items with emulator.Points with emulator.States with emulator.Worlds =>

  def genetic1(game: State): Commands = {
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
        val seq = Stream.continually(nextMove).takeWhile(x => x._2 != Abort).toList
        seq :+ (Invalid, Abort)
      }
      Stream.continually(genOne).iterator
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

    val chosenOne = p.head
    val commands = chosenOne map (_._2)
    println(commands mkString)
    println(eval(chosenOne))
    commands
  }
}