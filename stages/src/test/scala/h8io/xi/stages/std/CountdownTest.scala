package h8io.xi.stages.std

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CountdownTest extends AnyFlatSpec with Matchers with Inside {
//  "Countdown" should "return the state Complete after 3 iterations" in {
//    val initial = Countdown[Int](3, global = false)
//    inside(initial(42)) { case Yield.Some(42, onDone) =>
//      inside(onDone.onSuccess()) { case State.Success(stage) =>
//        inside(stage(17)) { case Yield.Some(17, onDone) =>
//          inside(onDone.onSuccess()) { case State.Success(stage) =>
//            inside(stage(3)) { case Yield.Some(3, onDone) =>
//              onDone.onSuccess() shouldBe State.Complete(initial)
//              onDone.dispose()
//            }
//          }
//        }
//      }
//    }
//  }
//
//  it should "return the state Complete on complete" in { testCompletedState(_.onComplete()) }
//  it should "return the state Complete on error" in { testCompletedState(_.onError()) }
//  it should "return the state Complete on panic" in { testCompletedState(_.onPanic()) }
//
//  private def testCompletedState(call: OnDone[Int, Int, Nothing] => State[Int, Int, Nothing]): Assertion = {
//    val initial = Countdown[Int](3, global = false)
//    inside(initial(42)) { case Yield.Some(42, onDone) =>
//      inside(onDone.onSuccess()) { case State.Success(stage) =>
//        inside(stage(17)) { case Yield.Some(17, onDone) =>
//          call(onDone) should matchPattern { case State.Complete(`initial`) => }
//        }
//      }
//    }
//  }
}
