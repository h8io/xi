package h8io.xi.stages.util

import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.Duration as jDuration
import scala.concurrent.duration.Duration

class GlobalSoftDeadlineTest
    extends AnyFlatSpec with Matchers with Inside with MockFactory with ScalaCheckPropertyChecks {
  "apply" should "create a GlobalSoftDeadline object from a duration" in
    forAll(Gen.choose(0, Long.MaxValue)) { nanos =>
      inside(GlobalSoftDeadline(Duration.fromNanos(nanos))) { case GlobalSoftDeadline(now, `nanos`) =>
        now() should be < now()
      }
      inside(GlobalSoftDeadline(jDuration.ofNanos(nanos))) { case GlobalSoftDeadline(now, `nanos`) =>
        now() should be < now()
      }
    }
}
