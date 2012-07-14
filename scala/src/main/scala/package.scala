package object icfp {
  def ??? = throw new RuntimeException

  type Emulator = emulator.Emulator
  type DumbEmulator = emulator.DumbEmulator
  type Strategies = strategies.Strategies
}