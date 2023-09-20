package digital.blueinsight.promptly

import scala.concurrent.{Await, ExecutionContext, Future}
import akka.stream.*
import akka.actor.*
import io.cequence.openaiscala.service.*
import io.cequence.openaiscala.domain.response.*
import io.cequence.openaiscala.domain.ModelId
import com.typesafe.config.*

import scala.concurrent.*
import scala.concurrent.duration.Duration

object Bookstore {
  case class Book(title: String, author: Author)
  case class Author(name: String, country: String)
  case class Review(book: Book, text: String)
  //Make sentiment a GADT
  case class EnrichedReview(review: Review, sentiment: Boolean = false)
  def enrichReview(review: Review): EnrichedReview = {
    ???
  }
  def calculateReviewScore(book: Book, reviews: List[EnrichedReview]): Int = {
    reviews.filter(_.review.book == book).count(_.sentiment)
  }
}
@main
def main(): Unit = {
  implicit val ec = ExecutionContext.global
  implicit val materializer = Materializer(ActorSystem())

  val config = ConfigFactory.load("openai-scala-client.conf")
  val service = OpenAIServiceFactory(config)

//  service.listModels.map(models =>
//    models.foreach(println)
//  )

//  val models: Future[Seq[ModelInfo]] = service.listModels

//  models.map(modelSeq => modelSeq.filter(_.owned_by=="openai").foreach(println))

//  service.retrieveModel(ModelId.text_davinci_003).map(model =>
//    println(model.getOrElse("N/A"))
//  )

  val text =
    """Extract the name and mailing address as a JSON with the fields "name" and "mailing_address" from this email:
      |Dear Kelly,
      |It was great to talk to you at the seminar. I thought Jane's talk was quite good.
      |Thank you for the book. Here's my address 2111 Ash Lane, Crestview CA 92002
      |Best,
      |Maya
             """.stripMargin

  println("executing completion")

  Await.result(service.createCompletion(text).map(completion =>
    println(completion.choices.head.text)
  ), Duration.Inf)


  service.close()

  println("Hello world!")
}