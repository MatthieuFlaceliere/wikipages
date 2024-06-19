import org.scalatest.funsuite.AnyFunSuite

class MainSpec extends AnyFunSuite {

  // test formatUrl
  test("test si l'URL est généré correctementL") {
    val keyword = "Scala"
    val limit = 10
    val expected =
      "https://en.wikipedia.org/w/api.php?action=query&format=json&prop=&sroffset=0&list=search&srsearch=Scala&srlimit=10"

    val result = Main.formatUrl(keyword, limit)
    assert(result == expected)
  }

  // test de la fonction ParseJson
  test("test analyse si le JSON valide") {
    val rawJson = """
      {
        "query": {
          "search": [
            { "title": "Scala", "wordcount": 100 },
            { "title": "Play Framework", "wordcount": 200 }
          ]
        }
      }
    """

    val expected = Seq(
      WikiPage("Scala", 100),
      WikiPage("Play Framework", 200)
    )

    val result = Main.parseJson(rawJson)
    assert(result == expected)
  }

  // Test de la fonction totalWords
  test("test avec une liste vide en entrée") {
    val pages = Seq.empty[WikiPage]
    val result = Main.totalWords(pages)
    assert(result == 0)
  }

  test("test avec une liste non vide") {
    val pages = Seq(
      WikiPage("Page 1", 100),
      WikiPage("Page 2", 200),
      WikiPage("Page 3", 150)
    )
    val result = Main.totalWords(pages)
    assert(result == 450)
  }

  // test de la fonction parseArguments
  test("test avec des arguments non parsable") {
    val args = Array("--invalid-option")
    val result = Main.parseArguments(args)
    assert(result.isEmpty)
  }

  test("Un avec un mot clef") {
    val args = Array("-k", "Scala")
    val expectedConfig = Config(keyword = "Scala")
    val result = Main.parseArguments(args)
    assert(result.contains(expectedConfig))
  }

  test("Un avec mot clef et limite") {
    val args = Array("-k", "Scala", "-l", "10")
    val expectedConfig = Config(limit = 10, keyword = "Scala")
    val result = Main.parseArguments(args)
    assert(result.contains(expectedConfig))
  }
}
