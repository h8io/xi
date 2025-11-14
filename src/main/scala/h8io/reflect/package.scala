package h8io

import scala.reflect.runtime.universe

package object reflect {
  implicit def tpe[T](implicit typeTag: universe.TypeTag[T]): Type[T] =
    new Type[T] {
      override private[reflect] val tag: universe.TypeTag[T] = typeTag
    }
}
