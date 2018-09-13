package mcgowan

case class Level(polish: String, english: String) {

  override def toString: String =
    "%s,%s,," format (polish, english)

}
