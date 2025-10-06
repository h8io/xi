package h8io.xi.stages.util

import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.{Duration, Instant}
import scala.concurrent.duration.FiniteDuration

class GlobalSoftDeadlineTest
    extends AnyFlatSpec with Matchers with Inside with MockFactory with ScalaCheckPropertyChecks {
  "apply" should "create a GlobalSoftDeadline object from an Instant object" in
    forAll { (deadline: Instant) =>
      inside(GlobalSoftDeadline(deadline)) { case GlobalSoftDeadline(now, `deadline`) =>
        now() should be < now()
      }
    }

  it should "create a GlobalSoftDeadline object from a finite duration" in
    forAll { (duration: FiniteDuration) =>
      val nanos = duration.toNanos
      val deadlineLowerBound = Instant.now().plusNanos(nanos)
      inside(GlobalSoftDeadline(duration)) { case GlobalSoftDeadline(now, deadline) =>
        deadline should be < Instant.now().plusNanos(nanos)
        deadline should be > deadlineLowerBound
        now() should be < now()
      }
    }

  it should "create a GlobalSoftDeadline object from a Java duration object" in
    forAll { (nanos: Long) =>
      val deadlineLowerBound = Instant.now().plusNanos(nanos)
      inside(GlobalSoftDeadline(Duration.ofNanos(nanos))) { case GlobalSoftDeadline(now, deadline) =>
        deadline should be < Instant.now().plusNanos(nanos)
        deadline should be > deadlineLowerBound
        now() should be < now()
      }
    }
}
