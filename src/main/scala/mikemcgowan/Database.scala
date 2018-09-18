package mikemcgowan

import Config._

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._

case class Database(polish: String, english: String, other: String, typ: String) {
  override def toString: String =
    "%s,%s,%s,%s" format (
      Util quote polish,
      Util quote english,
      Util quote other,
      Util quote typ
    )
}

object Database {
  private val browser  = new JsoupBrowser()
  private val filename = "memrise_database.csv"
  private val headers  = "polish,english,other,type\r\n"

  def scrape(): Unit = {
    browser setCookie ("", cookieKey, cookieVal)
    val pages = countPages()
    println("There are %d database pages to scrape ..." format pages)
    val dbData = (1 to pages) flatMap scrapeDatabasePage
    val sortedDbData = dbData sortBy (_.polish)
    Writer write (
      filename,
      headers,
      sortedDbData map ("%s\r\n" format _.toString)
    )
  }

  private def countPages(): Int = {
    val url = "%s/%s/%s/edit/database/%d/" format (memriseUrl, courseId, courseName, databaseId)
    val doc = browser get url
    val listItems = doc >> elementList("ul.pagination li")
    val pages = listItems map (_ >> allText("a"))
    pages.reverse.tail.head.toInt
  }

  private def scrapeDatabasePage(n: Int): List[Database] = {
    println("Scraping database page %d" format n)
    val url = "%s/%s/%s/edit/database/%d/?page=%d" format (
      memriseUrl,
      courseId,
      courseName,
      databaseId,
      n
    )
    val doc = browser get url
    val things = doc >> elementList("tr.thing")
    val selector = (m: Int) => "td:nth-child(%d) div.text" format m
    val as = things map (_ >> allText(selector(2)))
    val bs = things map (_ >> allText(selector(3)))
    val cs = things map (_ >> allText(selector(4)))
    val ds = things map (_ >> allText(selector(5)))
    (as zip bs zip cs zip ds) map {
      case (((w, x), y), z) => Database(w, x, y, z)
    }
  }
}
