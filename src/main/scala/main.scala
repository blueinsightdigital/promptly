package digital.blueinsight.promptly

import scala.concurrent.{Await, ExecutionContext, Future}
import akka.stream.*
import akka.actor.*
import io.cequence.openaiscala.service.*
import io.cequence.openaiscala.domain.response.*
import io.cequence.openaiscala.domain.ModelId
import com.typesafe.config.*
import digital.blueinsight.promptly.TicketAI.{ProductType, SentimentType, SubjectType, TagsType}
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
import cats.arrow.FunctionK
import cats.{Id, ~>}
import cats.free.Free
import cats.free.Free.liftF
import scala.collection.mutable

object GlobalConstants {
  val model = ModelId.gpt_3_5_turbo
}
object TicketStore {
  case class Ticket(message: String, author: String) derives ReadWriter
  case class EnrichedTicket(ticket: Ticket,
                            product: Option[String] = None,
                            subject: Option[String] = None,
                            sentiment: Option[String] = None,
                            tags:Option[List[String]] = None) derives ReadWriter

}
object TicketAI {
  sealed trait FieldType
  case object ProductType extends FieldType
  case object SubjectType extends FieldType
  case object SentimentType extends FieldType
  case object TagsType extends FieldType

  def getOutcome(ticket: TicketStore.Ticket, prompt: String, fieldType: FieldType): TicketStore.EnrichedTicket = {
    implicit val ec = ExecutionContext.global
    implicit val materializer = Materializer(ActorSystem())

    val config = ConfigFactory.load("openai-scala-client.conf")
    val service = OpenAIServiceFactory(config)

    println(prompt)


    val createChatCompletionSettings = CreateChatCompletionSettings(
      model = GlobalConstants.model
    )

    val messages: Seq[MessageSpec] = Seq(
      MessageSpec(role = ChatRole.System, content = "You are a helpful assistant."),
      MessageSpec(role = ChatRole.User, content = prompt)
    )

    val result = Await.result(service.createChatCompletion(
      messages = messages,
      settings = createChatCompletionSettings
    ).map { chatCompletion =>
      chatCompletion.choices.head.message.content
    }, Duration.Inf)

    service.close()

    fieldType match {
      case ProductType => TicketStore.EnrichedTicket(ticket = ticket,
        product = Some(result),
        subject = None,
        sentiment = None,
        tags = None
      )
      case SubjectType => TicketStore.EnrichedTicket(ticket = ticket,
        product = None,
        subject = Some(result),
        sentiment = None,
        tags = None
      )
      case SentimentType => TicketStore.EnrichedTicket(ticket = ticket,
        product = None,
        subject = None,
        sentiment = Some(result),
        tags = None
      )
      case TagsType => TicketStore.EnrichedTicket(ticket = ticket,
        product = None,
        subject = None,
        sentiment = None,
        tags = Some(List(result))
      )
      case _ => TicketStore.EnrichedTicket(ticket = ticket,
        product = None,
        subject = None,
        sentiment = None,
        tags = None
      )
    }
  }
}

object TicketFree {
  //1. Create an ADT to represent our grammar. Each rule corresponds to System, User, Assistant Messages.
  //Accumulated response is added back to the mutable Seq of Message Spec to continue the context.
  sealed trait ChatStoreA[A]
  case class SystemSays(content: String) extends ChatStoreA[Unit]
  case class UserSays(content: String) extends ChatStoreA[Unit]
  case class AssistantSays(content: String) extends ChatStoreA[Unit]

  case object ExecuteChat extends ChatStoreA[String]

//  Seq(
//    MessageSpec(role = ChatRole.System, content = "You are a helpful assistant."),
//    MessageSpec(role = ChatRole.User, content = "Who won the world series in 2020?"),
//    MessageSpec(role = ChatRole.Assistant, content = "The Los Angeles Dodgers won the World Series in 2020."),
//    MessageSpec(role = ChatRole.User, content = prompt))

  //2. Create a type based on Free[_] and KVStoreA[_]

  type ChatStore[A] = Free[ChatStoreA, A]

  //3. Create smart constructors for KVStore[_] using liftF.

  // SystemSays returns nothing (i.e. Unit). Mutable Seq is updated
  def systemSays(content: String): ChatStore[Unit] =
    liftF[ChatStoreA, Unit](SystemSays(content))

  def userSays(content: String): ChatStore[Unit] =
    liftF[ChatStoreA, Unit](UserSays(content))

  def assistantSays(content: String): ChatStore[Unit] =
    liftF[ChatStoreA, Unit](AssistantSays(content))

  def executeChat(): ChatStore[String] =
    liftF[ChatStoreA, String](ExecuteChat)

  // 3. Build a program
  // anywhere in the code

  // the program will crash if a type is incorrectly specified.
  def impureCompiler: ChatStoreA ~> Id = {
    new(ChatStoreA ~> Id) {

      // a very simple (and imprecise) chat archive
      var chats = scala.collection.mutable.ListBuffer[MessageSpec]()

      def apply[A](fa: ChatStoreA[A]): Id[A] = {
        fa match {
          case SystemSays(content) =>
            println(s"system says $content")
            chats += MessageSpec(role = ChatRole.System, content = content)
            ()
          case UserSays(content) =>
            println(s"user says $content")
            chats += MessageSpec(role = ChatRole.User, content = content)
            //get response and place it inside ?
            ()
          case AssistantSays(content) =>
            println(s"assistant says $content")
            chats += MessageSpec(role = ChatRole.Assistant, content = content)
            ()
          case ExecuteChat =>
            println(s"executing chat")
            helperChatAI(chats.toSeq).asInstanceOf[A]
        }
      }
    }
  }

    def getOutcome(program: ChatStore[String]) = {
      val outcome: Id[String] = program.foldMap(impureCompiler)
      outcome.asInstanceOf[String]
    }

    //helper function to execute the chat for a prompt
    def helperChatAI(messages: Seq[MessageSpec]): String = {
      implicit val ec = ExecutionContext.global
      implicit val materializer = Materializer(ActorSystem())

      val config = ConfigFactory.load("openai-scala-client.conf")
      val service = OpenAIServiceFactory(config)

      println(messages.length)

      val createChatCompletionSettings = CreateChatCompletionSettings(
        model = GlobalConstants.model
      )

      val result = Await.result(service.createChatCompletion(
        messages = messages,
        settings = createChatCompletionSettings
      ).map { chatCompletion =>
        chatCompletion.choices.head.message.content
      }, Duration.Inf)

      service.close()

      result
    }
}

object TicketsPlayground extends cask.MainRoutes {
  override def host: String = "0.0.0.0"

  override def port: Int = 80
  def commonHeaders = Seq("Access-Control-Allow-Origin" -> "http://127.0.0.1:5173")

  val originalTicket = TicketStore.Ticket(
    message = "I purchased this mixer from KitchenAid a few weeks ago and found it ridiculously useful to juice up my day",
    author = "Sisir Koppaka")

  @cask.get("/")
  def hello() =
    upickle.default.write(originalTicket)

  @cask.post("/read-request")
  def readRequest(request: cask.Request) = {
    val ticketRead = upickle.default.read[TicketStore.Ticket](request.text())
    upickle.default.write(ticketRead)
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

    upickle.default.write(TicketAI.getOutcome(ticketRead, prompt = prompt, fieldType = ProductType))
  }

  @cask.post("/summarize-data")
  def summarizeData(request: cask.Request) = {
    val ticketRead = upickle.default.read[TicketStore.Ticket](request.text())

    val promptEval =
      """Summarize the review below in a heading. Heading should be a short sentence of less than 15 words. Be creative, but be as close to the text below as possible:
        """+ticketRead.message+"""
        |
        |""".stripMargin

    upickle.default.write(TicketAI.getOutcome(ticketRead, prompt = promptEval, fieldType = SubjectType))
  }

  @cask.post("/determine-sentiment")
  def determineSentiment(request: cask.Request) = {
    val ticketRead = upickle.default.read[TicketStore.Ticket](request.text())

    val promptEval =
      """Determine the sentiment of the product review below. Be specific and answer with Positive, Negative, or Neutral. One word response only.
        """ + ticketRead.message +
        """
          |
          |""".stripMargin

    upickle.default.write(TicketAI.getOutcome(ticketRead, prompt = promptEval, fieldType = SentimentType))
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

    upickle.default.write(TicketAI.getOutcome(ticketRead, prompt = promptEval, fieldType = TagsType))
  }

  @cask.post("/map-reduce-summarize")
  def mapReduceSummarize(request: cask.Request) = {
    val ticketsRead = upickle.default.read[List[TicketStore.Ticket]](request.text())

    val ticketsSummary = ticketsRead.map( ticket => {
      val promptEval =
        """Summarize the review below in a short sentence. Be creative, but be as close to the text below as possible:
      """ + ticket.message +
          """
            |
            |""".stripMargin
      TicketAI.getOutcome(ticket, prompt = promptEval, fieldType = SubjectType)
    }).flatMap(_.subject).reduce((x,y) => x + "\n" + y)
    ticketsSummary
  }

  @cask.post("/map-reduce-set-tags")
  def mapReduceSetTags(request: cask.Request) = {
    val ticketsRead = upickle.default.read[List[TicketStore.Ticket]](request.text())

    val ticketsSummary = ticketsRead.map(ticket => {
      val promptEval =
        """Determine upto three tags for the below review. Be specific and descriptive and answer with upto 3 comma-separated one word tags only. Hypenate if requried to ensure each tag is mapped to a single contiguous word.
    """ + ticket.message +
          """
            |
            |""".stripMargin
      TicketAI.getOutcome(ticket, prompt = promptEval, fieldType = TagsType)
    }).flatMap(_.tags).toList.flatten.reduce((x, y) => x + "\n" + y)
    ticketsSummary
  }

  @cask.post("/map-reduce-sentiment")
  def mapReduceSentiment(request: cask.Request) = {
    val ticketsRead = upickle.default.read[List[TicketStore.Ticket]](request.text())

    val ticketsSummary = ticketsRead.map(ticket => {
      val promptEval =
        """Determine the sentiment of the product review below. Be specific and answer with Positive, Negative, or Neutral. One word response only.
          """ + ticket.message +
          """
            |
            |""".stripMargin
      TicketAI.getOutcome(ticket, prompt = promptEval, fieldType = SentimentType)
    }).flatMap(_.sentiment).reduce((x, y) => x + "\n" +  y)
    ticketsSummary
  }

  @cask.get("/free-outcome")
  def freeOutcome(request: cask.Request) = {
    import TicketFree._

    val program: ChatStore[String] =
      for {
        _ <- systemSays("You are a helpful assistant.")
        _ <- userSays("Who won the world series in 2020?")
        _ <- assistantSays("The Los Angeles Dodgers won the World Series in 2020.")
        _ <- userSays("Who won the most recent cricket world cup?")
        result <- executeChat()
      } yield result

    cask.Response(
      data = TicketFree.getOutcome(program),
      headers = commonHeaders)
  }

  initialize()
}