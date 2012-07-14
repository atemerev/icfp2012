package icfp
package strategies

trait Genetic1 {
  self: emulator.Commands with emulator.Games with emulator.Items with emulator.Points with emulator.States with emulator.Worlds =>

  def genetic1(game: State): Commands = {
    def initialSize = 50
    def ncrossover = initialSize / 3
    def maxLength = game.w.h * game.w.w / 2

    def seed = System.currentTimeMillis()
    val rng = new scala.util.Random(seed)
    def pickRandom[T](xs: T*): T = xs.toSeq(rng.nextInt(xs.toSeq.length))

    type TaggedSeq = List[(Point, Command)]
    def TaggedSeq(xs: (Point, Command)*) = List(xs: _*)
    val cache = collection.mutable.Map[TaggedSeq, Int]()
    def eval(s: TaggedSeq): Int = cache.getOrElseUpdate(s, playGame(game, s map (_._2)).score)
    implicit object ord extends Ordering[TaggedSeq] { def compare(s1: TaggedSeq, s2: TaggedSeq) = -(eval(s1).compare(eval(s2))) }
    import scala.collection.immutable.SortedSet
    type Population = SortedSet[TaggedSeq]
    def Population(xs: Traversable[TaggedSeq]) = SortedSet[TaggedSeq](xs.toSeq: _*)

    def mkPopulation(count: Int = initialSize): Population = Population(gen take count toList)

    def gen: Iterator[TaggedSeq] = {
      def genOne: TaggedSeq = {
        var points = collection.mutable.Set[Point]()
        var attempts = -1
        var g = game
        def nextMove: (Point, Command) = {
          attempts += 1
          if (attempts >= maxLength) (g.w.robot, Abort)
          else {
            val wannabe = pickRandom(Right, Left, Down, Up)
            g match {
              case ip: InProgress =>
                val nextState = ip.step(wannabe)
                val nextPos = nextState.w.robot
                val failed = points(nextPos)
                if (failed) nextMove else {
                  g = nextState
                  points += nextPos
                  (nextPos, wannabe)
                }
              case _ => (g.w.robot, Abort)
            }
          }
        }
        val seq = Stream.continually(nextMove).takeWhile(x => x._2 != Abort).toList
        val result = seq :+ (seq.last._1, Abort)
        cache(result) = (g match { case ip: InProgress => ip.step(Abort); case _ => g }).score
        result
      }
      Stream.continually(genOne).iterator
    }

    def evolve(p: Population): Population = {
      val allHybrids = for (a <- p.view; b <- p.view if (a.map(_._1).toSet intersect b.map(_._1).toSet).nonEmpty) yield crossover(a, b)
      val hybrids = allHybrids take ncrossover
      ((p ++ hybrids) take (p.size - ncrossover)) ++ mkPopulation(ncrossover)
    }

    def crossover(s1: TaggedSeq, s2: TaggedSeq): TaggedSeq = {
      val points = s1.map(_._1) intersect s2.map(_._1)
      val ixn = pickRandom(points: _*)

      val i1 = s1 indexWhere (_._1 == ixn)
      val i2 = s2 indexWhere (_._1 == ixn)
      // println(i1 + "-" + i2)
      (s1 take i1) ++ (s2 drop i2)
    }

    var p = mkPopulation()
    // println(p map (_ map (_._2) mkString) mkString "\n")
    for (i <- 0 to 20) {
      p = evolve(p)
      // println(i)
      // println(p map (_ map (_._2) mkString) mkString "\n")
    }

    // for ((seq, score) <- cache) {
    //   if (score != playGame(game, seq map (_._2)).score)
    //     throw new Exception("cache is rotten")
    // }

    val chosenOne = p.head
    val commands = chosenOne map (_._2)
    // println(commands mkString)
    // println(eval(chosenOne))
    commands
  }
}