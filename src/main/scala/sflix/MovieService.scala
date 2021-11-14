package sflix

import scala.xml.XML

// TODO refactor it so it can be used in tests (extract trait, default implementation) [2pt]
class MovieService(file: String) {

  val movies: Seq[Movie] = {
    // TODO refactor it so it is possible to add other file formats (i.e. JSON) [1pt]
    val doc = XML.loadFile(file)

    for {
      movie <- doc \ "movie"
    } yield {
      val id = (movie \@ "id").toInt
      val category = (movie \@ "category").toInt
      val title = (movie \ "title").text

      new Movie(id, title, category)
    }
  }

  def movieById(id: Int): Movie = {
    for (m <- movies) {
      if (m.id == id) return m
    }

    null
  }
}
