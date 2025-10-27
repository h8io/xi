package h8io.xi.cfg

trait Property[T] {
  def name: String
  def description: Option[String]

  def map[U](f: T => U): Property[U]
}
