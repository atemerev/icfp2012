package icfp
package strategies

trait Strategies extends Genetic1 with Pathfinder with Chess {
  self: Emulator =>
}