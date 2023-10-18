package digital.blueinsight.promptly

import scala.concurrent.{Await, ExecutionContext, Future}
import akka.stream.*
import akka.actor.*
import io.cequence.openaiscala.service.*
import io.cequence.openaiscala.domain.response.*
import io.cequence.openaiscala.domain.ModelId
import com.typesafe.config.*
import io.cequence.openaiscala.domain.settings.CreateCompletionSettings
import io.cequence.openaiscala.domain.{ChatRole, MessageSpec}
import io.cequence.openaiscala.domain.settings.CreateChatCompletionSettings
import scala.concurrent.*
import scala.concurrent.duration.Duration
import io.undertow.Undertow
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*
import io.circe.KeyDecoder.decodeKeyString
import upickle.default._

object TicketStore {
  case class Ticket(message: String, author: String) derives ReadWriter
  case class EnrichedTicket(ticket: Ticket,
                            product: Option[String],
                            subject: Option[String],
                            sentiment: Option[String],
                            tags:Option[List[String]]) derives ReadWriter

}
object TicketAI {
  def structuredData(ticket: TicketStore.Ticket, prompt: String): TicketStore.EnrichedTicket = {
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
    println(prompt)
//    val result = Await.result(service.createCompletion(ticket.message,
//      settings = CreateCompletionSettings(model = ModelId.gpt_3_5_turbo)
//    ).map(completion =>
//      (completion.choices.head.text)
//    ), Duration.Inf)

    val createChatCompletionSettings = CreateChatCompletionSettings(
      model = ModelId.gpt_3_5_turbo
    )

    val messages: Seq[MessageSpec] = Seq(
      MessageSpec(role = ChatRole.System, content = "You are a helpful assistant."),
//      MessageSpec(role = ChatRole.User, content = "Who won the world series in 2020?"),
//      MessageSpec(role = ChatRole.Assistant, content = "The Los Angeles Dodgers won the World Series in 2020."),
      MessageSpec(role = ChatRole.User, content = prompt),
    )

    val result = Await.result(service.createChatCompletion(
      messages = messages,
      settings = createChatCompletionSettings
    ).map { chatCompletion =>
      chatCompletion.choices.head.message.content
    }, Duration.Inf)

    service.close()

    TicketStore.EnrichedTicket(ticket = ticket,
      product = Some(result),
      subject = None,
      sentiment = None,
      tags = None
    )
  }
}

object TicketsPlayground extends cask.MainRoutes {
  override def host: String = "0.0.0.0"

  override def port: Int = 80

  val originalTicket = TicketStore.Ticket(
    message = "I purchased this mixer from KitchenAid a few weeks ago and found it ridiculously useful to juice up my day",
    author = "Sisir Koppaka")

  @cask.get("/")
  def hello() =
    upickle.default.write(originalTicket)

  @cask.post("/read-request")
  def readRequest(request: cask.Request) = {
    val ticketRead = upickle.default.read[TicketStore.Ticket](request.text())
    upickle.default.write(ticketRead.author)
  }

  @cask.post("/structured-data")
  def structuredData(request: cask.Request) = {
    val ticketRead = upickle.default.read[TicketStore.Ticket](request.text())

    val prompt =
      """Extract the name of the product being talked about. Be as specific as possible, and include the brand name and the product name. Reply with just the name as text.
        | I purchased this mixer from KitchenAid a few weeks ago and found it ridiculously useful to juice up my day.
        |""".stripMargin

    val promptJSON =
      """Extract the name of the product being talked about. Be as specific as possible, and include the brand name and the product name. Reply in JSON with the field name "name" from this customer service email:
        | I purchased this mixer from KitchenAid a few weeks ago and found it ridiculously useful to juice up my day.
        |""".stripMargin

    upickle.default.write(TicketAI.structuredData(ticketRead, prompt = prompt))
  }

  @cask.post("/summarize-data")
  def summarizeData(request: cask.Request) = {
    val ticketRead = upickle.default.read[TicketStore.Ticket](request.text())
//
//    val prompt =
//      """Summarize the review below in a short sentence. Be creative, but be as close to the text below as possible:
//        | If you're a beginner baker like myself, I highly recommend this artisan model. It's user friendly, and I like how it came with the three different attachments and a pouring shield as well. Also the 5 quart is honestly enough room for an at home baker. However, if you plan to be baking huge batches for a commercial business this one may not be for you and you may want to opt out for a bigger mixer.
//        |
//        |""".stripMargin

    val promptEval =
      """Summarize the review below in a short sentence. Be creative, but be as close to the text below as possible:
        """+ticketRead.message+"""
        |
        |""".stripMargin


    upickle.default.write(TicketAI.structuredData(ticketRead, prompt = promptEval))
  }

  @cask.post("/determine-sentiment")
  def determineSentiment(request: cask.Request) = {
    val ticketRead = upickle.default.read[TicketStore.Ticket](request.text())

    val promptEval =
      """Determine the sentiment of the product review below. Be specific and answer with positive, negative, or neutral. One word response only.
        """ + ticketRead.message +
        """
          |
          |""".stripMargin

    upickle.default.write(TicketAI.structuredData(ticketRead, prompt = promptEval))
  }

  @cask.post("/set-tags")
  def setTags(request: cask.Request) = {
    val ticketRead = upickle.default.read[TicketStore.Ticket](request.text())

    val promptEval =
      """Determine upto three tags for the below review. Be specific and descriptive and answer with upto 3 comma-separated one word tags only. Hypenate if requried to ensure each tag is mapped to a single contiguous word.
        """ + ticketRead.message +
        """
          |
          |""".stripMargin

    upickle.default.write(TicketAI.structuredData(ticketRead, prompt = promptEval))
  }

  initialize()
}