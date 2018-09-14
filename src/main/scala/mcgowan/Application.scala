package mcgowan

object Application {

  def main(args: Array[String]): Unit = {
    Config.cookieVal = args(0)
    Level scrape()
    Database scrape()
  }

}
