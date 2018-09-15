package mcgowan

object Util {
  def quote(s: String): String =
    if (s contains ',')
      '"' + s + '"'
    else
      s
}
