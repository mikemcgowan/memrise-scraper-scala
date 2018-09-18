package mikemcgowan

import Config._

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._

case class Level(polish: String, english: String) {
  override def toString: String =
    "%s,%s,," format (
      Util quote polish,
      Util quote english
    )
}

object Level {
  private val browser  = JsoupBrowser()
  private val filename = "memrise_levels.csv"
  private val headers  = "polish,english,other,type\r\n"

  def scrape(): Unit = {
    val levels = countLevels()
    println("There are %d levels to scrape ..." format levels)
    val levelData = (1 to levels) flatMap scrapeLevel
    val sortedLevelData = levelData sortBy (_.polish)
    Writer write (
      filename,
      headers,
      sortedLevelData map ("%s\r\n" format _.toString)
    )
  }

  private def countLevels(): Int = {
    val url = "%s/%s/%s/" format (memriseUrl, courseId, courseName)
    val doc = browser get url
    val levelItems = doc >> elementList("a.level")
    levelItems.size
  }

  private def scrapeLevel(l: Int): List[Level] = {
    println("Scraping level %d" format l)
    val url = "%s/%s/%s/%d/" format (memriseUrl, courseId, courseName, l)
    val doc = browser get url
    val things = doc >> elementList("div .thing")
    val as = things map (_ >> allText(".col_a div.text"))
    val bs = things map (_ >> allText(".col_b div.text"))
    (as zip bs) map {
      case (x, y) => Level(x, y)
    }
  }
}
