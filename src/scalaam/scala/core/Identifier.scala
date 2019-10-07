package scalaam.core

/** An identifier. It has a name and a position */
case class Identifier(name: String, pos: Position) extends SmartHash {
  override def toString = s"$name@${pos.column}:${pos.line}" // Apparently, column is the line no. and pos is the column no.
}
