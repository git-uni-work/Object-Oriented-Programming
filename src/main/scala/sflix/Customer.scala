package sflix

import scala.xml.XML
import scala.collection.mutable.ListBuffer

/**
 *
 * @param movieId id of the movie
 * @param quality streaming quality which can be one of
 *   - `1` - Standard Definition (SD)
 *   - `2` - High Definition (HD)
 *   - `3` - 4K (4K)
 */

class Streaming(val movieId: Int, val quality: Int)

object Customer
{
  // TODO refactor it so it is possible to add other file formats (i.e. JSON) [1pt]
  def load(file: String): Seq[Customer] = {
    val doc = XML.loadFile(file)
    for {
      customer <- doc \ "customer"
    } yield {
      val id = (customer \@ "id").toInt
      val name = customer \@ "name"
      val streamings = for {
        s <- customer \\ "streaming"
        movieId = s \@ "movie"
        quality = s \@ "quality"
      } yield new Streaming(movieId.toInt, quality.toInt)

      // TODO remove global state [1pt]
      // TODO remove global state [1pt]
      new Customer(id, name, streamings,movieService = null , false , false )
    }
  }
}

// class structure of all the movies watched by a customer and their calculated prices / qualities
class movieswatched ( val title : String , val quality : String , val calcprice : Double )
// class structure of the streaming summary of a customer
class summary ( val streams : Int , val movies : Int , val totalprice : Double , val points : Int )

// " Movie Service " & " Loyalty Points " & " Use Emphasis " added to the constructor of a Customer as private variables
class Customer (val id: Int, val name: String, val streamings: Seq[Streaming] , private var movieService: MovieService , private var LoyaltyPointsBonus : Boolean , private var UseEmphasis : Boolean ) {

  // list of all the movies watched by a customer and their details
  var listofmovies = new ListBuffer[movieswatched]()
  // summary of the streams of a customer
  var sum : summary = null

  // getters & setters for the customers private variables
  def setmovieservice ( x : MovieService ) : Unit =
  { movieService = x }
  def setloyaltypointsbonus ( x : Boolean ) : Unit =
  { LoyaltyPointsBonus = x }
  def setemphasis ( x : Boolean ) : Unit =
  { UseEmphasis = x }
  def getloyaltypointsbonus () : Boolean =
  { LoyaltyPointsBonus }
  def getemphasis () : Boolean =
  { UseEmphasis }
  def getmovieservice () : MovieService =
  { movieService }

  // TODO this method does too much, refactor the statement computation and formatting [4pt]
  // statement function for the computation & storing of the customer's usage values
  def statement(): Unit =
  {
    var total: Double = 0.0
    var loyaltyPoints: Int = 0

    for ( streaming <- streamings) {
      var price: Double = 0
      var qualitySurcharge: Double = 1.0
      var qualityText: String = null
      val movie = movieService.movieById(streaming.movieId)

      if ( movie != null )
      {
        qualitySurcharge = 1.0
        if ( movie.category == 1 || movie.category == 3)
        {
          price = 2
          loyaltyPoints += 1
          if ( movie.category == 1 && streaming.quality == 2 )
          { qualitySurcharge = 1.25 ; qualityText = "HD" ; loyaltyPoints += 1  }
          if ( movie.category == 1 && streaming.quality == 3 )
          { qualitySurcharge = 1.5 ; qualityText = "4K" ; loyaltyPoints += 1 }
          if ( movie.category == 3 && streaming.quality == 2 )
          { qualityText = "HD" ; loyaltyPoints += 1 }
          if ( movie.category == 3 && streaming.quality == 3 )
          { qualityText = "4K" ; loyaltyPoints += 1 }
        }
        else
        {
          price = 3
          loyaltyPoints += 1
          if ( streaming.quality == 2 )
          { qualitySurcharge = 1.5 ; qualityText = "HD" ; loyaltyPoints += 1 }
          if ( streaming.quality == 3 )
          { qualitySurcharge = 1.75 ; qualityText = "4K" ; loyaltyPoints += 1 }
        }
        total += price * qualitySurcharge

        // creates a new instance of the movies watched by a customer & their details then adds them to a list
        val watched : movieswatched = new movieswatched(movie.title,qualityText,price*qualitySurcharge)
        listofmovies += watched
        listofmovies.toList
      }
    }

    if (streamings.size >= 2)
    { loyaltyPoints += streamings.size - 2  }
    if (LoyaltyPointsBonus)
    { loyaltyPoints *= 2  }

    var uniqueMovies = 0
    var seenMovies = scala.collection.mutable.Set[Int]()
    for (streaming <- streamings) {
      if (!seenMovies.contains(streaming.movieId)) {
        uniqueMovies += 1
        seenMovies.add(streaming.movieId)
      }
    }

    // creates an instance of the summary of a customer's streams
    sum = new summary(streamings.size,uniqueMovies,total,loyaltyPoints)

  }

  // creates a statement of TXT format
  def textstatement() : String =
  {
    var txt : String = null
    statement()
    if (UseEmphasis)
    { txt = f"Streaming report for *$name%s* ($id%d)\n" }
    else
    { txt = f"Streaming report for $name%s ($id%d)\n" }

    for ( movie <- listofmovies )
    {
      txt += f"- ${movie.title}%s"
      if (movie.quality != null)
      { txt += f" (${movie.quality}%s )" }
      txt += " "
      txt += s"${movie.calcprice}%d CZK\n"
    }

    txt += (f"\nStreamings: ${sum.streams}%d\n")
    txt += (f"Movies: ${sum.movies}%d\n")
    if (UseEmphasis)
    { txt += (f"Total: *${sum.totalprice}%f* CZK\n") }
    else
    { txt += (f"Total: ${sum.totalprice}%f CZK\n") }
    txt += (f"Points: ${sum.points}%d\n" )
    txt
  }

  // creates a statement of HTML format
  def htmlstatement() : String =
  {
    var html : String = null
    statement()
    html = "<html>\n<body>\n"
    html += f"<h1>Streaming report for ${name}%s (${id}%d)</h1>\n"

    html += "<ul>\n"
    for ( movie <- listofmovies )
    {
      html += f"<li>${movie.title}%s"
      if (movie.quality != null)
      { html += f" (${movie.quality}%s)" }
      html += " "
      html += f"${movie.calcprice}%2.1f CZK</li>\n"
    }
    html += "</ul>\n"

    html += "<br/>"
    html += (f"\nStreamings: ${sum.streams}%d<br/>\n")
    html += (f"Movies: ${sum.movies}%d<br/>\n")
    if (UseEmphasis)
    { html += (f"Total: <b>${sum.totalprice}%2.1f</b> CZK<br/>\n") }
    else
    { html += (f"Total: ${sum.totalprice}%2.1f CZK<br/>\n") }
    html += (f"Points: ${sum.points}%d\n" )

    html += "</body>\n</html>\n"
    html
  }

}
