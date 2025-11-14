package h8io.reflect

trait Type[T] {
  private[reflect] val tag: scala.reflect.runtime.universe.TypeTag[T]

  final def <:<(other: Type[?]): Boolean = tag.tpe <:< other.tag.tpe

  override final def toString: String = tag.tpe.dealias.toString
}
