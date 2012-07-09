package icfpc

import org.specs.runner.JUnit4
import org.specs.Specification

class SomeTest extends JUnit4(SomeTest)

object SomeTest extends Specification {
  "Our wonderful team" should {
    "win" in {
      true must beTrue
    }
  }
}
