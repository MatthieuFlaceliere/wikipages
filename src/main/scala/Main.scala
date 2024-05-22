import scopt.OParser
import scalaj.http.Http
import scala.util.Try
import play.api.libs.json.{Json, JsArray}

case class WikiPage(title: String, words: Int)

case class Config(limit: Int = 10, keyword: String = "")

object Main extends App {
  parseArguments(args) match {
    case Some(config) => run(config)
    case _            => println("Unable to parse arguments")
  }

  def parseArguments(args: Array[String]): Option[Config] = {
    val builder = OParser.builder[Config]
    val parser = {
      import builder._
      OParser.sequence(
        programName("WikiStats"),
        opt[Int]('l', "limit")
          .action((x, c) => c.copy(limit = x))
          .text("limit is an integer property"),
        opt[String]('k', "keyword")
          .required()
          .action((x, c) => c.copy(keyword = x))
          .text("keyword is a string property")
      )
    }

    OParser.parse(parser, args, Config())
  }

  def run(config: Config): Unit = {
    val url = formatUrl(config.keyword, config.limit)
    getPages(url) match {
      case Right(body) => {
        val pages = parseJson(body)
        pages.foreach { page =>
          println(s"${page.title} - ${page.words}")
        }

        val totalWords = this.totalWords(pages)
        println(s"Total words: $totalWords")
        val avgWords = totalWords / pages.length
        println(s"Average words: $avgWords")
      }
      case Left(code) => println(s"Error: HTTP code $code")
    }
  }

  def formatUrl(keyword: String, limit: Int): String = {
    s"https://en.wikipedia.org/w/api.php?action=query&format=json&prop=&sroffset=0&list=search&srsearch=${keyword}&srlimit=${limit}"
  }

  def getPages(url: String): Either[Int, String] = {
    Try {
      val response = Http(url).asString
      if (response.is2xx) {
        Right(response.body)
      } else {
        Left(response.code)
      }
    }.getOrElse(Left(500))
  }

  def parseJson(rawJson: String): Seq[WikiPage] = {
    val json = Json.parse(rawJson)

    val searchResults = (json \ "query" \ "search").as[JsArray].value

    searchResults.map { result =>
      val title = (result \ "title").as[String]
      val words = (result \ "wordcount").as[Int]
      WikiPage(title, words)
    }
  }

  def totalWords(pages: Seq[WikiPage]): Int = {
    pages.foldLeft(0)((acc, page) => acc + page.words)
  }
}
