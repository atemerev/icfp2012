package icfp
package emulator

trait Emulator extends Commands with Games with Items with Points with States with Worlds

trait DumbEmulator extends Emulator with DumbWorlds

// (xb to vp): is yet to come
//trait SmartEmulator extends Emulator with SmartWorlds