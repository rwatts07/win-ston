package code.comet

import scala.xml.Text
import scala.collection.mutable.Map
import net.liftweb.common.{Box,Full,Empty}
import net.liftweb.actor.LiftActor
import net.liftweb.util.Schedule
import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import scala.io.Source

final case object YourTurn

final case class AttemptJoin(openGame: OpenGame, who: CometActor)

final case class Choose(pile: Pile, from: CometActor) 
final case class Pass(pile: Pile, from: CometActor)

final case class Piles(deckCount: Int, piles: List[Pile]) // list of piles Game sends to CometActors along with # of cards left in deck 

final case class TakePile(pile: Pile)

final case object UpdatePicks

final case class AddPlayerWithGame(who: CometActor, game: Game)
final case class RemovePlayer(who: CometActor)
final case class NowPlaying(game: Game)
final case object ResetGame
final case object LeaveGame
final case object ReturnToLobby

object Lobby extends LiftActor {
  private var games: List[Game] = Nil
  private var waiting: Map[OpenGame, CometActor] = Map()
  
  
  def messageHandler = {
    case AttemptJoin(openGame, player) => {
      waiting.get(openGame) match {
        case Some(p) => {
          val opps = List(player, p)
          val game = new Game(player, p, openGame.cards)
          opps.foreach(_ ! NowPlaying(game))
          waiting = waiting - openGame
        }
        case None => {
          waiting = waiting + (openGame -> player)
        }
      }
    }
    case RemovePlayer(who) =>
      waiting = waiting.filter(_ ne who)
  }
  
}

class Deck(cardList: List[String]) {

  private var cards = cardList

  def count: Int = cards.length

  def draw: List[String] = draw(1)

  def draw(n: Int = 1): List[String] = {
    val drawn = cards.take(n)
    cards = cards.drop(n)
    drawn
  }
}

class Game(playerOne: CometActor, playerTwo: CometActor, cards: List[String] = CardPool.cardPool) extends LiftActor {


  val players = List(playerOne, playerTwo)
  val cardPool: List[String] = cards

  private var deck = new Deck(scala.util.Random.shuffle(cardPool).take(100))

  private var piles = startPiles

  sendToAllPlayers(Piles(deck.count, piles))

  private var activePlayer = playerOne
  activePlayer ! YourTurn

  private def nextPlayer = if (activePlayer == playerOne) playerTwo else playerOne

  def startPiles: List[Pile] = {
    val pileCards: List[String] = deck.draw(3)
    val pile1 = Pile(List(pileCards(0)), true, 1)
    val pile2 = Pile(List(pileCards(1)), false, 2)
    val pile3 = Pile(List(pileCards(2)), false, 3)
    List(pile1, pile2, pile3)
  }
  
  private def sendToAllPlayers(msg: Any){
    playerOne ! msg
    playerTwo ! msg
  }
  
  def ignoreEmptyPiles(ps: List[Pile]): List[Pile] = {

   if (ps.forall(_.cards.isEmpty)) {
     List()
   }
   else if (ps(0).current && ps(0).cards.isEmpty) {
     ignoreEmptyPiles(List(ps(0).copy(current = false), ps(1).copy(current = true), ps(2)))
   }
   else if (ps(1).current && ps(1).cards.isEmpty) {
     ignoreEmptyPiles(List(ps(0), ps(1).copy(current = false), ps(2).copy(current = true)))
    }
    else if (ps(2).current && ps(2).cards.isEmpty) {
      ignoreEmptyPiles(List(ps(0).copy(current = true), ps(1), ps(2).copy(current = false)))
    }
    else {
      ps
    }
  }
     
  
  def messageHandler = {
    case Choose(pile, from) => {
      piles = pile.id match {
        case 1 => List(Pile(deck.draw, true, 1),
                        piles(1),
                        piles(2))
        case 2 => List(piles(0).copy(current = true),
                       Pile(deck.draw, false, 2),
                       piles(2))
        case 3 => List(piles(0).copy(current = true),
                       piles(1),
                       Pile(deck.draw, false, 3))
      }
      activePlayer = nextPlayer
      activePlayer ! YourTurn

      piles = ignoreEmptyPiles(piles)

      sendToAllPlayers(Piles(deck.count, piles))
    } 
    case Pass(pile, from) => {
      piles = pile.id match {
        case 1 => {
          List(Pile(deck.draw ::: pile.cards, false, 1),
                        piles(1).copy(current = true),
                        piles(2))
        }
        case 2 => List(piles(0),
                       Pile(deck.draw ::: pile.cards, false, 2),
                       piles(2).copy(current = true))
        case 3 => {
          List(piles(0).copy(current = true),
               piles(1),
              Pile(deck.draw ::: pile.cards, false, 3)) }
        
      }
      if (pile.id == 3) {
        activePlayer ! TakePile(Pile(deck.draw, false, 4))
        activePlayer = nextPlayer
        activePlayer ! YourTurn
      }

      piles = ignoreEmptyPiles(piles)

      sendToAllPlayers(Piles(deck.count, piles))
    } 
    case ResetGame => 
      sendToAllPlayers(ResetGame)
      
    case LeaveGame => 
      sendToAllPlayers(ReturnToLobby)
      
  }
}

case class Pile(cards: List[String], current: Boolean, id: Int)

object DeckList extends SessionVar[Box[String]](Empty)
object FileName extends SessionVar[Box[String]](Empty)
object CurrentGame extends SessionVar[Box[OpenGame]](Empty)

class WinstonDraft extends CometActor {

  private var openGame: Box[OpenGame] = CurrentGame.is
  private var game: Box[Game] = Empty
  private var main: List[String] = List()
  private var side: List[String] = List()
  private var piles: List[Pile] = List()
  private var deckCount: Int = 90
  private var isActive: Boolean = false

  private def faceDown(card: String) =  <img width="240" height="340" class="pile" src="images/Back.full.jpg" />
  
  override def mediumPriority = {
    case TakePile(p) =>
      main = p.cards ::: main
    case Piles(dc, p) =>
      piles = p
      deckCount = dc
      reRender(true)
    case NowPlaying(g) => 
      game = Full(g)
      reRender(true)
    case YourTurn => 
      isActive = true
      reRender(true)
    case ResetGame => 
      reRender(true)
    case UpdatePicks =>
      reRender(true)
    case ReturnToLobby => {
      game = Empty
      DeckList.set(Empty)
      FileName.set(Empty)
      CurrentGame.set(Empty)
      LobbyActor.is.foreach(LobbyServer ! JoinLobby(_))
      partialUpdate(RedirectTo("/draftLobby.html"))
    }
  }

  def rawImageUrls(cards: List[String]) = 
    cards map {
      card => <img width="240" height="340" class="pile" src={cardImage(card)} />
    }

  def render = {
    if(!game.isEmpty && !piles.isEmpty) {
      "#deckCount" #> Text("Cards Left in Pool: %s".format(deckCount)) &
      ".pileRow *" #> piles.map { pile =>
        ".pile" #> { if (pile.current && isActive) rawImageUrls(pile.cards) else pile.cards.map(faceDown) } &
         ".choose" #> {
           if (pile.current && isActive) 
             SHtml.ajaxButton("Choose", choosePile(pile)) 
           else
             SHtml.ajaxButton("Choose", choosePile(pile), "disabled" -> "disabled")
         } &
         ".pass" #> {
           if (pile.current && isActive) 
             SHtml.ajaxButton("Pass", passPile(pile)) 
           else
             SHtml.ajaxButton("Pass", passPile(pile), "disabled" -> "disabled")
         }
                                 } &
      mainTableSeq
    }
    else if (!game.isEmpty && piles.isEmpty) {
      mainTableSeq &
      "#download" #> downloadMain
    }
    else {
      "#game *" #> "Waiting in the lobby for an opponent..."
    }
  }

  def mainTableSeq = {
    landsForm &
    "#mainHeader" #> Text("Main %s".format(main.length)) &
    ".mainRow *" #> main.grouped(4).toList.map { grp => mainRowSeq(grp) } &
      "#sideHeader" #> Text("Side %s".format(side.length)) &
      ".sideRow *" #> side.grouped(4).toList.map { grp => sideRowSeq(grp) } &
    "#leave" #> SHtml.ajaxButton("Leave", leave)
  }

  def mainRowSeq(group: List[String]) = {
    ".mainCard *" #> group.map { card =>
      "img [src]" #> cardImage(card) &
      "img [onclick]" #> SHtml.ajaxInvoke(() => moveToSide(card))
             }
  }

  def sideRowSeq(group: List[String]) = {
    ".sideCard *" #> group.map { card =>
      "img [src]" #> cardImage(card) &
      "img [onclick]" #> SHtml.ajaxInvoke(() => moveToMain(card))
             }
  }

  def landsForm = {
    "#mountains" #> SHtml.ajaxText(main.filter(_ == "Mountain").length.toString, (s: String) => addLands(s, "Mountain")) &
    "#island" #> SHtml.ajaxText(main.filter(_ == "Island").length.toString, s => addLands(s, "Island")) &
    "#plains" #> SHtml.ajaxText(main.filter(_ == "Plains").length.toString, s => addLands(s, "Plains")) &
    "#forest" #> SHtml.ajaxText(main.filter(_ == "Forest").length.toString, s => addLands(s, "Forest")) &
    "#swamp" #> SHtml.ajaxText(main.filter(_ == "Swamp").length.toString, s => addLands(s, "Swamp")) 
  }
 
  def leave = () => {
    game match {
      case Full(g) => {
        g ! LeaveGame
        Noop
      }
      case _ => {
        Noop
      }
    }
  }

  def moveToSide(card: String) = {
    if (main.contains(card)) { // Avoids bug with double-clicking
      main = main.filterNot(_ == card)
      side = card :: side
      this ! UpdatePicks
    }
    Noop
  }

   def moveToMain(card: String) = {
    if (side.contains(card)) { // Avoids bug with double-clicking
      side = side.filterNot(_ == card)
      main = card :: main
      this ! UpdatePicks
    }
    Noop
  }

  def toInt(s: String):Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e:Exception => None
    }
  }

  def addLands(s: String, landType: String) = {
    toInt(s) match {
      case Some(n) => {
        val lands = (1 to n).map(_ => landType).toList
        main = main.filterNot(_ == landType) // Remove pre-existing lands
        main = lands ::: main
        this ! UpdatePicks
        Noop
      }
      case None => Alert("Number of lands must be a number")
    }
  }

  def downloadMain = {
    val m = main.map(card => "1 " + card).mkString("\n")
    val s  = side.map(card => "SB: 1 " + card).mkString("\n")
    val deckString = m + "\n" + s
    var fname = ""

    SHtml.ajaxForm(
      <p>Save As: { SHtml.text("", n => fname = n) }</p> ++
      { SHtml.ajaxSubmit("Download", () => {
        DeckList.set(Full(deckString))
        FileName.set(Full(fname))
        RedirectTo("/download")
      })})

  }

  def choosePile(pile: Pile) = () => {
    main = main ::: pile.cards
    game.foreach(_ ! Choose(pile, this))
    isActive = false
    reRender(true)
    _Noop
  }
  
  def passPile(pile: Pile) = () => {
    if (pile.id == 3) {
      isActive = false
      reRender(true)
    }
    game.foreach(_ ! Pass(pile, this))
    _Noop
  }

  def cardImage(name: String) = {
    val fixedName = if (name contains "//") name.takeWhile(_ != ' ') else name // Fix for Split cards
    
   "http://www.mtgimage.com/card/%s.jpg".format(name.replace(" ", "_"))
  }

  override def lifespan: Box[TimeSpan] = Full(2 minutes)
  
  override def localSetup(){
    DraftActor.set(Full(this))
    openGame match {
      case Full(g) => {
        Lobby ! AttemptJoin(g, this)
      }
      case _ => println(openGame)
    }
    super.localSetup()  
  }

  override def localShutdown() {
    Lobby ! RemovePlayer(this)
    super.localShutdown()
  }
}


object CardPool {
  val cardPool = 
    List("Elite Vanguard",
         "Isamaru, Hound of Konda",
         "Mother of Runes",
         "Savannah Lions",
         "Soldier of the Pantheon",
         "Steppe Lynx",
         "Student of Warfare",
         "Accorder Paladin",
         "Imposing Sovereign",
         "Knight of Meadowgrain",
         "Kor Skyfisher",
         "Lone Missionary",
         "Soltari Monk",
         "Soltari Priest",
         "Soltari Trooper",
         "Stoneforge Mystic",
         "Thalia, Guardian of Thraben",
         "Wall of Omens",
         "Blade Splicer",
         "Flickerwisp",
         "Mirran Crusader",
         "Mirror Entity",
         "Soltari Champion",
         "Exalted Angel",
         "Hero of Bladehold",
         "Kor Sanctifiers",
         "Ranger of Eos",
         "Restoration Angel",
         "Baneslayer Angel",
         "Cloudgoat Ranger",
         "Karmic Guide",
         "Reveillark",
         "Sun Titan",
         "Elesh Norn, Grand Cenobite",
         "Eternal Dragon",
         "Ajani Goldmane",
         "Elspeth, Knight-Errant",
         "Gideon Jura",
         "Elspeth, Sun's Champion",
         "Porcelain Legionnaire",
         "Enlightened Tutor",
         "Mana Tithe",
         "Path to Exile",
         "Swords to Plowshares",
         "Disenchant",
         "Balance",
         "Spectral Procession",
         "Armageddon",
         "Day of Judgment",
         "Ravages of War",
         "Wrath of God",
         "Catastrophe",
         "Entreat the Angels",
         "Land Tax",
         "Journey to Nowhere",
         "Seal of Cleansing",
         "Oblivion Ring",
         "Faith's Fetters",
         "Moat",
         "Spear of Heliod",
         "Enclave Cryptologist",
         "Looter il-Kor",
         "Phantasmal Image",
         "Snapcaster Mage",
         "Waterfront Bouncer",
         "Man-o'-War",
         "Serendib Efreet",
         "Trinket Mage",
         "Vendilion Clique",
         "Glen Elendra Archmage",
         "Sower of Temptation",
         "Venser, Shaper Savant",
         "Meloku the Clouded Mirror",
         "Morphling",
         "Mulldrifter",
         "Aetherling",
         "Consecrated Sphinx",
         "Jace Beleren",
         "Jace, the Mind Sculptor",
         "Tezzeret the Seeker",
         "Phyrexian Metamorph",
         "Inkwell Leviathan",
         "Crystal Shard",
         "Vedalken Shackles",
         "Force of Will",
         "Ancestral Recall",
         "Brainstorm",
         "Force Spike",
         "Mystical Tutor",
         "Arcane Denial",
         "Counterspell",
         "Daze",
         "Impulse",
         "Mana Drain",
         "Mana Leak",
         "Memory Lapse",
         "Miscalculation",
         "Remand",
         "Capsize",
         "Forbid",
         "Cryptic Command",
         "Fact or Fiction",
         "Gifts Ungiven",
         "Condescend",
         "Ponder",
         "Preordain",
         "Time Walk",
         "Timetwister",
         "Tinker",
         "Deep Analysis",
         "Bribery",
         "Time Spiral",
         "Upheaval",
         "Control Magic",
         "Opposition",
         "Treachery",
         "Thassa, God of the Sea",
         "Academy Ruins",
         "Faerie Conclave",
         "Tolarian Academy",
         "Carnophage",
         "Diregraf Ghoul",
         "Gravecrawler",
         "Tormented Hero",
         "Vampire Lacerator",
         "Blood Scrivener",
         "Bloodghast",
         "Dark Confidant",
         "Nezumi Graverobber",
         "Oona's Prowler",
         "Pack Rat",
         "Reassembling Skeleton",
         "Vampire Hexmage",
         "Bane of the Living",
         "Geralf's Messenger",
         "Hypnotic Specter",
         "Vampire Nighthawk",
         "Braids, Cabal Minion",
         "Nekrataal",
         "Skinrender",
         "Ob Nixilis, the Fallen",
         "Shriekmaw",
         "Grave Titan",
         "Griselbrand",
         "Tombstalker",
         "Liliana of the Veil",
         "Contagion",
         "Snuff Out",
         "Dark Ritual",
         "Entomb",
         "Vampiric Tutor",
         "Diabolic Edict",
         "Go for the Throat",
         "Dismember",
         "Hero's Downfall",
         "Duress",
         "Imperial Seal",
         "Inquisition of Kozilek",
         "Reanimate",
         "Thoughtseize",
         "Chainer's Edict",
         "Demonic Tutor",
         "Exhume",
         "Hymn to Tourach",
         "Sinkhole",
         "Smallpox",
         "Pox",
         "Yawgmoth's Will",
         "Consuming Vapors",
         "Damnation",
         "Mind Twist",
         "Profane Command",
         "Sarcomancy",
         "Animate Dead",
         "Bitterblossom",
         "Dance of the Dead",
         "Necromancy",
         "Phyrexian Arena",
         "Recurring Nightmare",
         "Volrath's Stronghold",
         "Firedrinker Satyr",
         "Goblin Guide",
         "Goblin Welder",
         "Gorilla Shaman",
         "Grim Lavamancer",
         "Jackal Pup",
         "Mogg Fanatic",
         "Reckless Waif",
         "Spikeshot Elder",
         "Stromkirk Noble",
         "Ash Zealot",
         "Gore-House Chainwalker",
         "Hellspark Elemental",
         "Keldon Marauders",
         "Lightning Mauler",
         "Plated Geopede",
         "Stormblood Berserker",
         "Young Pyromancer",
         "Blistering Firecat",
         "Fire Imp",
         "Imperial Recruiter",
         "Keldon Vandals",
         "Manic Vandal",
         "Squee, Goblin Nabob",
         "Avalanche Riders",
         "Flametongue Kavu",
         "Hellrider",
         "Hero of Oxid Ridge",
         "Siege-Gang Commander",
         "Thundermaw Hellkite",
         "Inferno Titan",
         "Greater Gargadon",
         "Koth of the Hammer",
         "Chandra Nalaar",
         "Burst Lightning",
         "Firestorm",
         "Lightning Bolt",
         "Incinerate",
         "Lightning Strike",
         "Magma Jet",
         "Blast from the Past",
         "Char",
         "Staggershock",
         "Chain Lightning",
         "Faithless Looting",
         "Firebolt",
         "Arc Trail",
         "Arc Lightning",
         "Flames of the Firebrand",
         "Molten Rain",
         "Pillage",
         "Wheel of Fortune",
         "Burning of Xinye",
         "Earthquake",
         "Rolling Earthquake",
         "Wildfire",
         "Bonfire of the Damned",
         "Sulfuric Vortex",
         "Sneak Attack",
         "Purphoros, God of the Forge",
         "Arbor Elf",
         "Birds of Paradise",
         "Elvish Mystic",
         "Experiment One",
         "Fyndhorn Elves",
         "Joraga Treespeaker",
         "Jungle Lion",
         "Llanowar Elves",
         "Noble Hierarch",
         "Wild Dogs",
         "Wild Nacatl",
         "Wolfbitten Captive",
         "Fauna Shaman",
         "Lotus Cobra",
         "River Boa",
         "Rofellos, Llanowar Emissary",
         "Sakura-Tribe Elder",
         "Scavenging Ooze",
         "Strangleroot Geist",
         "Sylvan Caryatid",
         "Tarmogoyf",
         "Vinelasher Kudzu",
         "Wall of Blossoms",
         "Wall of Roots",
         "Wild Mongrel",
         "Eternal Witness",
         "Nantuko Vigilante",
         "Troll Ascetic",
         "Uktabi Orangutan",
         "Viridian Shaman",
         "Yavimaya Elder",
         "Master of the Wild Hunt",
         "Polukranos, World Eater",
         "Thrun, the Last Troll",
         "Vengevine",
         "Wickerbough Elder",
         "Acidic Slime",
         "Deranged Hermit",
         "Genesis",
         "Kalonian Hydra",
         "Thragtusk",
         "Primeval Titan",
         "Hornet Queen",
         "Woodfall Primus",
         "Garruk Relentless",
         "Garruk Wildspeaker",
         "Garruk, Primal Hunter",
         "Worldly Tutor",
         "Life from the Loam",
         "Regrowth",
         "Call of the Herd",
         "Cultivate",
         "Kodama's Reach",
         "Natural Order",
         "Plow Under",
         "Green Sun's Zenith",
         "Rancor",
         "Survival of the Fittest",
         "Sylvan Library",
         "Treetop Village",
         "Geist of Saint Traft",
         "Venser, the Sojourner",
         "Supreme Verdict",
         "Detention Sphere",
         "Celestial Colonnade",
         "Flooded Strand",
         "Hallowed Fountain",
         "Tundra",
         "Psychatog",
         "Shadowmage Infiltrator",
         "Tezzeret, Agent of Bolas",
         "Baleful Strix",
         "Creeping Tar Pit",
         "Polluted Delta",
         "Underground Sea",
         "Watery Grave",
         "Rakdos Cackler",
         "Falkenrath Aristocrat",
         "Murderous Redcap",
         "Dreadbore",
         "Badlands",
         "Blood Crypt",
         "Bloodstained Mire",
         "Lavaclaw Reaches",
         "Kird Ape",
         "Bloodbraid Elf",
         "Sarkhan Vol",
         "Xenagos, the Reveler",
         "Raging Ravine",
         "Stomping Ground",
         "Taiga",
         "Wooded Foothills",
         "Dryad Militant",
         "Qasali Pridemage",
         "Voice of Resurgence",
         "Kitchen Finks",
         "Horizon Canopy",
         "Savannah",
         "Temple Garden",
         "Windswept Heath",
         "Desolation Angel",
         "Sorin, Lord of Innistrad",
         "Lingering Souls",
         "Vindicate",
         "Caves of Koilos",
         "Godless Shrine",
         "Marsh Flats",
         "Scrubland",
         "Lotleth Troll",
         "Abrupt Decay",
         "Maelstrom Pulse",
         "Pernicious Deed",
         "Bayou",
         "Llanowar Wastes",
         "Overgrown Tomb",
         "Verdant Catacombs",
         "Edric, Spymaster of Trest",
         "Trygon Predator",
         "Mystic Snake",
         "Simic Sky Swallower",
         "Breeding Pool",
         "Misty Rainforest",
         "Temple of Mystery",
         "Tropical Island",
         "Ral Zarek",
         "Fire // Ice",
         "Izzet Charm",
         "Steam Augury",
         "Izzet Boilerworks",
         "Scalding Tarn",
         "Steam Vents",
         "Volcanic Island",
         "Figure of Destiny",
         "Ajani Vengeant",
         "Boros Charm",
         "Lightning Helix",
         "Arid Mesa",
         "Battlefield Forge",
         "Plateau",
         "Sacred Foundry",
         "Karn Liberated",
         "Phyrexian Revoker",
         "Masticore",
         "Molten-Tail Masticore",
         "Solemn Simulacrum",
         "Triskelion",
         "Wurmcoil Engine",
         "Myr Battlesphere",
         "Sundering Titan",
         "Black Lotus",
         "Blacker Lotus",
         "Chrome Mox",
         "Everflowing Chalice",
         "Mana Crypt",
         "Mox Diamond",
         "Mox Emerald",
         "Mox Jet",
         "Mox Pearl",
         "Mox Ruby",
         "Mox Sapphire",
         "Black Vise",
         "Bonesplitter",
         "Cursed Scroll",
         "Mana Vault",
         "Pithing Needle",
         "Sensei's Divining Top",
         "Skullclamp",
         "Sol Ring",
         "Ankh of Mishra",
         "Coldsteel Heart",
         "Grim Monolith",
         "Mind Stone",
         "Scroll Rack",
         "Umezawa's Jitte",
         "Winter Orb",
         "Coalition Relic",
         "Crucible of Worlds",
         "Grafted Wargear",
         "Mimic Vat",
         "Ring of Gix",
         "Sword of Body and Mind",
         "Sword of Feast and Famine",
         "Sword of Fire and Ice",
         "Sword of Light and Shadow",
         "Sword of War and Peace",
         "Tangle Wire",
         "Worn Powerstone",
         "Erratic Portal",
         "Icy Manipulator",
         "Nevinyrral's Disk",
         "Smokestack",
         "Batterskull",
         "Engineered Explosives",
         "Memory Jar",
         "Ancient Tomb",
         "City of Ass",
         "City of Brass",
         "Dust Bowl",
         "Evolving Wilds",
         "Gemstone Mine",
         "Grand Coliseum",
         "Library of Alexandria",
         "Maze of Ith",
         "Mishra's Factory",
         "Mutavault",
         "Reflecting Pool",
         "Rishadan Port",
         "Strip Mine",
         "Undiscovered Paradise",
         "Wasteland")
}
