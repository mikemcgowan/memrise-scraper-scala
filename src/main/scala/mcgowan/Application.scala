package mcgowan

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._

object Application {

  val memriseUrl = "https://www.memrise.com/course"
  val levels     = 5 // 42
  val courseId   = 1049040
  val courseName = "mikes-polish-course"

  val browser = JsoupBrowser()

  def main(args: Array[String]): Unit = {
    scrapeLevels()
  }

  def scrapeLevels(): Unit = {
    val levelData = 1 to levels flatMap extractLevel
    val sortedLevelData = levelData sortBy (_.polish)
    sortedLevelData foreach println
  }

  def extractLevel(l: Int): List[Level] = {
    val url = "%s/%s/%s/%d/" format (memriseUrl, courseId, courseName, l)
    val doc = browser get url
    val things = doc >> elementList("div .thing")
    val as = things map (_ >> allText(".col_a div.text"))
    val bs = things map (_ >> allText(".col_b div.text"))
    (as zip bs) map (t => Level(t._1, t._2))
  }

}
