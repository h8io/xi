package h8io.reflect

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util as ju

class TypeTest extends AnyFlatSpec with Matchers {
  "Type" should "be summoned and compared for a primitive type" in {
    implicitly[Type[Int]] <:< implicitly[Type[Any]] shouldBe true
    implicitly[Type[Int]] <:< implicitly[Type[AnyVal]] shouldBe true
    implicitly[Type[Int]] <:< implicitly[Type[AnyRef]] shouldBe false
    implicitly[Type[Int]] <:< implicitly[Type[Integer]] shouldBe false
    implicitly[Type[Integer]] <:< implicitly[Type[Int]] shouldBe false
  }

  it should "be summoned and compared for a plain reference type" in {
    implicitly[Type[String]] <:< implicitly[Type[AnyVal]] shouldBe false
    implicitly[Type[String]] <:< implicitly[Type[AnyRef]] shouldBe true
    implicitly[Type[String]] <:< implicitly[Type[CharSequence]] shouldBe true
    implicitly[Type[String]] <:< implicitly[Type[Nothing]] shouldBe false
    implicitly[Type[CharSequence]] <:< implicitly[Type[String]] shouldBe false
  }

  it should "be summoned and compared for a reference type with invariant type parameters" in {
    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.Collection[CharSequence]]] shouldBe false
    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.Collection[String]]] shouldBe true
    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.Collection[Nothing]]] shouldBe false
    implicitly[Type[ju.List[CharSequence]]] <:< implicitly[Type[ju.Collection[String]]] shouldBe false

    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.List[CharSequence]]] shouldBe false
    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.List[String]]] shouldBe true
    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.List[Nothing]]] shouldBe false
    implicitly[Type[ju.List[CharSequence]]] <:< implicitly[Type[ju.List[String]]] shouldBe false

    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.ArrayList[CharSequence]]] shouldBe false
    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.ArrayList[String]]] shouldBe false
    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.ArrayList[Nothing]]] shouldBe false
    implicitly[Type[ju.List[CharSequence]]] <:< implicitly[Type[ju.ArrayList[String]]] shouldBe false
  }

  it should "be summoned and compared for a reference type with covariant type parameters" in {
    implicitly[Type[Seq[String]]] <:< implicitly[Type[Iterable[CharSequence]]] shouldBe true
    implicitly[Type[Seq[String]]] <:< implicitly[Type[Iterable[String]]] shouldBe true
    implicitly[Type[Seq[String]]] <:< implicitly[Type[Iterable[Nothing]]] shouldBe false
    implicitly[Type[Seq[CharSequence]]] <:< implicitly[Type[Iterable[String]]] shouldBe false

    implicitly[Type[Seq[String]]] <:< implicitly[Type[Seq[CharSequence]]] shouldBe true
    implicitly[Type[Seq[String]]] <:< implicitly[Type[Seq[String]]] shouldBe true
    implicitly[Type[Seq[String]]] <:< implicitly[Type[Seq[Nothing]]] shouldBe false
    implicitly[Type[Seq[CharSequence]]] <:< implicitly[Type[Seq[String]]] shouldBe false

    implicitly[Type[Seq[String]]] <:< implicitly[Type[List[CharSequence]]] shouldBe false
    implicitly[Type[Seq[String]]] <:< implicitly[Type[List[String]]] shouldBe false
    implicitly[Type[Seq[String]]] <:< implicitly[Type[List[Nothing]]] shouldBe false
    implicitly[Type[Seq[CharSequence]]] <:< implicitly[Type[List[String]]] shouldBe false
  }

  it should "be summoned and compared for a reference type with contravariant type parameters" in {
    implicitly[Type[PartialFunction[String, Unit]]] <:< implicitly[Type[CharSequence => Unit]] shouldBe false
    implicitly[Type[PartialFunction[String, Unit]]] <:< implicitly[Type[String => Unit]] shouldBe true
    implicitly[Type[PartialFunction[String, Unit]]] <:< implicitly[Type[Nothing => Unit]] shouldBe true
    implicitly[Type[PartialFunction[CharSequence, Unit]]] <:< implicitly[Type[String => Unit]] shouldBe true

    implicitly[Type[PartialFunction[String, Unit]]] <:< implicitly[
      Type[PartialFunction[CharSequence, Unit]]] shouldBe false
    implicitly[Type[PartialFunction[String, Unit]]] <:< implicitly[Type[PartialFunction[String, Unit]]] shouldBe true
    implicitly[Type[PartialFunction[String, Unit]]] <:< implicitly[Type[PartialFunction[Nothing, Unit]]] shouldBe true
    implicitly[Type[PartialFunction[CharSequence, Unit]]] <:< implicitly[
      Type[PartialFunction[String, Unit]]] shouldBe true

    implicitly[Type[String => Unit]] <:< implicitly[Type[PartialFunction[CharSequence, Unit]]] shouldBe false
    implicitly[Type[String => Unit]] <:< implicitly[Type[PartialFunction[String, Unit]]] shouldBe false
    implicitly[Type[String => Unit]] <:< implicitly[Type[PartialFunction[Nothing, Unit]]] shouldBe false
    implicitly[Type[CharSequence => Unit]] <:< implicitly[Type[PartialFunction[String, Unit]]] shouldBe false
  }
}
