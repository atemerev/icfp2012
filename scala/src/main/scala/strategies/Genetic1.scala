package icfp
package strategies

import collection.SeqView

trait Genetic1 {
  self: emulator.Commands with emulator.Games with emulator.Items with emulator.Points with emulator.States with emulator.Worlds with Pathfinder =>

  def genetic1(game: State, trace: Boolean = false): Commands = {
    def initialSize = 70
    def iterations = 100
    def ncrossover = initialSize / 3
    def maxLength = game.w.h * game.w.w

    def seed = System.currentTimeMillis()
    val rng = new scala.util.Random(seed)
    def pickRandom[T](xs: T*): T = xs.toSeq(rng.nextInt(xs.toSeq.length))

    type TaggedSeq = List[(Point, Command)]
    val cache = collection.mutable.Map[TaggedSeq, State]()
    val scores2 = collection.mutable.Map[TaggedSeq, Int]()
    val mods = collection.mutable.Map[Point, Int]()
    implicit object ord extends Ordering[TaggedSeq] {
      def compare(s1: TaggedSeq, s2: TaggedSeq) =
        if (s1 == s2) 0 else if (eval(s2) - eval(s1) > 0) 1 else -1
    }
    def eval(s: TaggedSeq): Int = {
      val final_state = cache.getOrElseUpdate(s, playGame(game, s map (_._2)))
      var unmodded = final_state.score
      val finishAccessible: Boolean = if (final_state.w.liftIsOpen && final_state.w.robot != Invalid) {
        val distanceToLift = mkDistMap(final_state.w, final_state.w.lift)(final_state.w.robot.x)(final_state.w.robot.y)
        if (distanceToLift == Int.MaxValue) false
        else {
          unmodded += final_state.collectedLambdas*50 - distanceToLift
          true
        }
      } else false
      var mod = final_state.w.remainingLambdaPositions.map(pos => -mods.getOrElse(pos, 0)).sum / 10
      mod += (if (finishAccessible) 100000 else 0)
      scores2 += (s -> unmodded)
      unmodded + mod.toInt
    }

    import scala.collection.immutable.SortedSet
    type Population = SortedSet[TaggedSeq]
    def Population(xs: Traversable[TaggedSeq]) = SortedSet[TaggedSeq](xs.toSeq: _*)

    val history = collection.mutable.Set[TaggedSeq]()
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
            var possibleMoves = List(Right, Left, Up)
            if (g.w(g.w.robot + (0, -1)) != Rock) possibleMoves :+= Down

            val wannabe = pickRandom(possibleMoves:_*)
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
              case _ => (Invalid, Abort)
            }
          }
        }
        val seq = Stream.continually(nextMove).takeWhile(x => x._2 != Abort).toList
        val result = g match {
          case _ : InProgress => seq :+ (seq.last._1, Abort)
          case _ => seq
        }
        if (history(result)) genOne else {
          history += result
          cache(result) = g match { case ip: InProgress => ip.step(Abort); case _ => g }
          result
        }
      }
      Stream.continually(genOne).iterator
    }

    def evolve(p: Population): Population = {
      val allHybrids = for (a <- p.view; b <- p.view) yield {
        def qq(x: TaggedSeq): Int = cache(x).w.robot match {
          case Invalid => -1000
          case _ => cache(x).w.robot.distanceTo(cache(x).w.lift)
        }
        // val factor = if (cache(a).w.liftIsOpen || cache(b).w.liftIsOpen) 300/(qq(a) + qq(b)) else 5
        val factor = 10
        val authorizeMarriage = (a.map(_._1).toSet intersect b.map(_._1).toSet).nonEmpty && rng.nextInt(100) < factor
        if (authorizeMarriage) crossover(a, b) else List()
      }
      (Population(p ++ allHybrids.flatten) take (p.size - ncrossover)) ++ mkPopulation(ncrossover)
    }

    def crossover(s1: TaggedSeq, s2: TaggedSeq): Seq[TaggedSeq] = {
      val points = s1.map(_._1) intersect s2.map(_._1)

      val result = for (ixn <- points take 5) yield {
        val i1 = s1 indexWhere (_._1 == ixn)
        val i2 = s2 indexWhere (_._1 == ixn)
        //if (trace) {
        // println(i1 + "-" + i2)
        // }
        (s1 take i1) ++ (s2 drop i2)
      }
      val filtered = result.filter(x => eval(x) > eval(s1) && eval(x) > eval(s2))
      filtered
    }

    var p = mkPopulation()
    if (trace) {
      //println(p map (_ map (_._2) mkString) mkString "\n")
    }
    for (i <- 0 to iterations) {
      val won = cache.get(p.head).map(_.isInstanceOf[Won]).getOrElse(false)
      if (!won) {
        p = evolve(p)

        mods.clear()
        p foreach (seq => cache(seq).w.remainingLambdaPositions foreach (pos => {
          val prev = mods.getOrElse(pos, 0)
          mods(pos) = prev + cache(seq).score
        }))
        if (trace) {
          println(mods)
        }

        if (trace) {
          println()
          println("Iteration " + i + ", best score: real = " + scores2(p.head) + ", modded = " + eval(p.head) + "\n" + playGame(game, p.head.map(_._2)).w.toString)
          //println(p map (_ map (_._2) mkString) mkString "\n")
        }
      }
    }

    // for ((seq, state) <- cache) {
    //   if (state.score != playGame(game, seq map (_._2)).score)
    //     throw new Exception("cache is rotten")
    // }

    val chosenOne = p.head
    val commands = chosenOne map (_._2)
    if (trace) {
      println(commands mkString)
      println(cache(chosenOne).score)
    }
    commands
  }
}