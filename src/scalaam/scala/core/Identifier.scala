package scalaam.core

/** An identifier. It has a name and a position */
case class Identifier(name: String, pos: Position) extends SmartHash {
  override def toString = s"$name@${pos.line}:${pos.column}"
}
