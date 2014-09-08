package code.comet

import scala.xml.Text
import scala.collection.mutable.Map
import net.liftweb.common.{Box,Full,Empty}
import net.liftweb.actor.LiftActor
import net.liftweb.util.Schedule
import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JsCmd
import scala.io.Source
import net.liftweb.json._
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model.GetObjectRequest
import com.amazonaws.auth.BasicAWSCredentials

final case class OpenGame(name: String, cards: List[String], player: CometActor)
final case class AddGame(game: OpenGame)
final case class JoinLobby(who: CometActor)
final case class JoinGame(game: OpenGame, who: CometActor)
final case class UpdateGames(games: List[OpenGame])
final case class StartGame(game: OpenGame)
final case class WaitingOnGame(game: OpenGame)
final case class LeaveOpenGame(game: OpenGame, who: CometActor)

object LobbyServer extends LiftActor {
  
  private var games: List[OpenGame] = Nil
  private var players: List[CometActor] = Nil
  private var waitingForOpponent: List[CometActor] = Nil

  def messageHandler = {
    case AddGame(game: OpenGame) => {
      val creator = game.player
      waitingForOpponent ::= creator
      creator ! WaitingOnGame(game)
      players = players.filterNot(_ == creator)
      games ::= game 
      players.foreach(_ ! UpdateGames(games))
    }
    case JoinLobby(player: CometActor) => {
      players ::= player
      players.foreach(_ ! UpdateGames(games))
    }
    case JoinGame(game: OpenGame, who: CometActor) => {
      val toJoin = List(game.player, who)
      waitingForOpponent = waitingForOpponent.filterNot(_ == game.player)
      toJoin.foreach(_ ! StartGame(game))
      games = games.filterNot(_ == game)
      players = players diff toJoin
      players.foreach(_ ! UpdateGames(games))    
    }
    case LeaveOpenGame(game: OpenGame, who: CometActor) => {
      games = games.filterNot(_ == game)
      waitingForOpponent = waitingForOpponent.filterNot(_ == who)
      players ::= who
      players.foreach(_ ! UpdateGames(games))
    }
  }
}
  

class DraftLobby extends CometActor {

  private var games: List[OpenGame] = List()
  
  private def waitingOnGame: Boolean = !CurrentGame.is.isEmpty

  def render = {
    if (!waitingOnGame) {
      ".gameRow *" #> games.map { game =>
        ".name" #> Text(game.name) &
        ".join" #> SHtml.ajaxButton("Join", join(game)) } &
        "#create" #> SHtml.ajaxButton("Create", create) 
    }
    else {
      "#game" #> Text("Waiting for an opponent...") &
      "#leave" #> SHtml.ajaxButton("Leave", leave)
    }
  }
  
  def leave = () => {
    CurrentGame.is match {
      case Full(g) => {
        CurrentGame.set(Empty)
        LobbyServer ! LeaveOpenGame(g, this)
        Noop
      }
      case _ => {
        RedirectTo("/draftLobby.html")
      }
    }
  }

  def join(game: OpenGame) = () => {
    games = games.filterNot(_ == game)
    LobbyServer ! JoinGame(game, this) 
    Noop
  }

  def create = () => {
     ask(new AskCards, "What cards?") {
      case (n, cs) => {
        val name = n.toString
        val cards = cs.toString
        LobbyServer ! AddGame(OpenGame(name, cards.split("\n").toList, this))
        reRender(true)
      }
      case _ => {
        S.notice("Invalid cards")
        reRender(true)
      }
    }
    Noop
  }

  override def mediumPriority = {
    case UpdateGames(gs: List[OpenGame]) => {
      games = gs
      reRender(true)
    }
    case StartGame(openGame: OpenGame) => {
      CurrentGame.set(Full(openGame))
      partialUpdate(RedirectTo("/winston-draft.html"))
    }
    case WaitingOnGame(game: OpenGame) => {
      CurrentGame.set(Full(game))
      reRender(true)
    }
  }

  override def localSetup() {
    LobbyServer ! JoinLobby(this)
    CurrentGame.set(Empty)
    super.localSetup()
  }
}

class AskCards extends CometActor {
  private var nickname: String = ""
  private var cards: String = ""

  def render = {
      SHtml.ajaxForm(
        <p>Name <br />{
          SHtml.text("", n => nickname = n)}</p> ++
        <p>Cards: <br />{
          SHtml.textarea("", c => cards = c, "rows" -> "90", "cols" -> "30")}</p> ++
        { SHtml.ajaxSubmit("Create", () => validateAndAnswer(nickname, cards)) })
  }

  def cleanCardList(cards: String): List[String] = {
    
     val splitRegex = " // .*".r

    val cardList: List[String] = { for {
      card <- cards.split("\n")
      noSplit = splitRegex.replaceAllIn(card, "")
      } yield noSplit.trim } toList

    cardList
  }
    

  def validateAndAnswer(name: String, cards: String) = {

    val cardList = cleanCardList(cards)

    val invalidCards = validateCards(cardList)

    if (invalidCards.isEmpty) {
      answer((name, cardList.mkString("\n")))
    }
    else {
      S.notice("Invalid cards: " + invalidCards.mkString("\n"))
    }
  }

  
    
    
  def validateCards(cardList: List[String]): List[String] = {
     
    val validCards = ValidCards.validCards

    cardList.filterNot(card => validCards.contains(card)).toList
  } 
}

object ValidCards {
  // Download Cards from S3

  val bucketName = "cardjson"

  val awsId = "AKIAJYFJPRZKHY2HHMLA"
  val awsKey = "lbMSnsnddhFOOqerxp7BxCGADTYB0T5+5UV6tYPp"
  val credentials = new BasicAWSCredentials(awsId, awsKey)
  val conn = new AmazonS3Client(credentials)

  val cardsObj = conn.getObject(new GetObjectRequest(bucketName, "AllSetsArray.json"))

  val cardsData = cardsObj.getObjectContent()

  val cardsStr = scala.io.Source.fromInputStream(cardsData).mkString

  val json = parse(cardsStr)
  
  val singleCards = for { JField("name", JString(name)) <- json } yield name

  val splitCards = for { JField("names", JArray(names)) <- json; firstNameStr = names.head; JString(firstName) <- firstNameStr } yield firstName
     
  val validCards = (singleCards ::: splitCards).distinct // Get rid of split notation  
} 
