package sflix

import org.scalatest.{FunSuite, Matchers}
import scala.io.Source

// TODO refactor tests so there is no code duplicate [1pt]
class CustomerTest extends FunSuite with Matchers {

  test("Customer 1 TXT-statement")
  {
    val movieService = new MovieService(getClass.getResource("/movies.xml").getFile)
    val customers = Customer.load(getClass.getResource("/customers.xml").getFile)

    customers(0).setmovieservice(movieService)
    customers(0).setemphasis(true)

    val fileContents = Source.fromFile("customer1.txt").getLines.mkString

    // TODO save the expected outputs into files [1pt]
    customers(0).textstatement() shouldBe fileContents
  }

  // TODO fix the test [1pt]
  test("Customer 2 TXT-statement")
  {
    val movieService = new MovieService(getClass.getResource("/movies.xml").getFile)
    val customers = Customer.load(getClass.getResource("/customers.xml").getFile)

    customers(1).setmovieservice(movieService)
    customers(1).setemphasis(false)
    customers(1).setloyaltypointsbonus(true)

    val fileContents = Source.fromFile("customer2.txt").getLines.mkString
    customers(1).textstatement() shouldBe fileContents
  }

  // TODO create a test that asserts an HTML report [3pt]
  test("Customer 2 HTML-statement")
  {
    val movieService = new MovieService(getClass.getResource("/movies.xml").getFile)
    val customers = Customer.load(getClass.getResource("/customers.xml").getFile)

    customers(1).setmovieservice(movieService)
    customers(1).setemphasis(true)
    customers(1).setloyaltypointsbonus(false)
    customers(1).htmlstatement() shouldBe
      """<html>
        |<body>
        |<h1>Streaming report for John Doe (2)</h1>
        |<ul>
        |<li>Irishman 3.0 CZK</li>
        |<li>Irishman (HD) 4.5 CZK</li>
        |<li>Taxi Driver (4K) 3.0 CZK</li>
        |</ul>
        |<br/>
        |Streamings: 3<br/>
        |Movies: 2<br/>
        |Total: <b>10.5</b> CZK<br/>
        |Points: 6
        |</body>
        |</html>
        |""".stripMargin
  }
  // TODO create a test that asserts correct price computation [1pt]
  // TODO create a test that asserts correct loyalty points computation [1pt]
}
