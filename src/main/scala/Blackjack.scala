import scala.util.Random;
import scala.io.StdIn.readLine;
import scala.io.StdIn.readInt;
import scala.util.Properties;
import scala.annotation.tailrec;
object Blackjack {

  //states of the Game
  enum State:
    case Start, SelectHands, PlaceBets, InitialDeal, CheckDealerBJ, Playing, PlayDealer, Outcome, Reshuffle, End

  //Classes that define objects used for the blackjack game.
  class GameInfo(
    val deck : List[Cards.Card],
    val deckindex : Int = 1,
    val pen : Int = 50,
    val minbet : Double = 5,
    val maxbet : Double = 300)
  class GameState(
    val state : State,
    val dealer : List[Cards.Card] = List(),
    val players : List[Player],
    val info : GameInfo)
  class Hand(
    val hand : List[Cards.Card] = List(),
    val bet : Double = 0,
    val betplaced : Boolean = false,
    val stand : Boolean = false,
    val payout : Double = 0)
  class Player(
    val name : String,
    val bankroll : Double,
    val hands : List[Hand] = List(),
    val handsplaying : Int = 0)

  //global line seperator that is used accross outputs.
  val sep = Properties.lineSeparator 

  /*
  	function to build a stadard double deck blackjack game
	with a penetration of around 64% - 75%
  */
  def buildDoubleDeck : GameInfo =
    val random = new scala.util.Random
    val dd = random.shuffle(Cards.deck++Cards.deck)
    val pen = random.between(67,78)
    GameInfo(dd,1,pen,5,300)

  /*
  	function to calculate the blackjack hand total
	card values are defined in Cards.scala
  */
  def handTotal(hand : Hand) : Int = 
    handTotal(hand.hand)
  def handTotal(hand : List[Cards.Card]) : Int =
    val acecount = hand.filter(_.facename == "Ace").map(_.value).length
    val nonaces = hand.filter(_.facename != "Ace").map(_.value)
    val acepart = acecount match
      case 0 => 0
      case _ => if nonaces.sum + 11 + acecount-1 < 22 then 11 + acecount-1 else acecount
    nonaces.sum + acepart

  //simple function to see if hand has blackjack
  def blackjack(hand : List[Cards.Card]) : Boolean =
    hand.length == 2 match
      case true => 
        val card1 = hand(0) 
        val card2 = hand(1) 
        card1.value + card2.value == 21 
      case false => false

  def findcurrplayer(gs : GameState) : Option[Player] = 
    gs.state match
      case State.SelectHands =>
        gs.players.find(player => player.hands.length == 0)
      case State.PlaceBets =>
        gs.players.filterNot(player => player.hands.forall(hand => hand.betplaced)).headOption
      case State.Playing =>
        gs.players.filterNot(player => player.hands.forall(hand => hand.stand)).headOption
      case _ => None

  def selecthands(player : Player, num : Int) : Player =
    val hands = List.fill(num)(Hand())
    Player(player.name,player.bankroll,hands,num)

  def addtobet(player : Player, info : GameInfo, choice : Int) : Player =
    val hand = player.hands.filter(hand => !hand.betplaced).head
    val musthave = ((player.hands.filter(hand => !hand.betplaced).length) - 1) * info.minbet
    val newhand = 
      choice match
        case 1 => if (player.bankroll-5) - musthave >= 0 then Hand(hand.hand,hand.bet+5,false,hand.stand,hand.payout)
                  else hand
        case 2 => if (player.bankroll-25) - musthave >= 0 then Hand(hand.hand,hand.bet+25,false,hand.stand,hand.payout)
                  else hand
        case 3 => if (player.bankroll-50) - musthave >= 0 then Hand(hand.hand,hand.bet+50,false,hand.stand,hand.payout)
                  else hand
        case 4 => if (player.bankroll-100) - musthave >= 0 then Hand(hand.hand,hand.bet+100,false,hand.stand,hand.payout)
                  else hand
        case 5 => if hand.bet > 0 then Hand(hand.hand, hand.bet, true, false, hand.payout) else hand
        case 6 => Hand(hand.hand,0,false,hand.stand,hand.payout)
        case 7 => Hand(hand.hand,0,true,hand.stand,hand.payout)
        case _ => hand
    val newhands = player.hands
                         .zipWithIndex
                         .map{case(h, idx) =>
                           if idx == player.hands.indexOf(hand) then newhand
                           else h}
                         .filterNot(hand => hand.bet == 0 && hand.betplaced)
    val moneyspent = newhand.bet - hand.bet
    Player(player.name, player.bankroll - moneyspent, newhands, player.handsplaying)

  def play_output(gs : GameState) : String =
    gs.state match
      case State.Start =>
        //starting message
        "Welcome to double deck blackjack." + sep + "Press Enter to Play."
      case State.SelectHands =>
        //lists all the players info
        val playersinfo = gs.players
                          .map(player => player.name + "\t$" + player.bankroll)
                          .mkString(sep)
        //find the current player and prompt them to select number of hands
        val currplayer = findcurrplayer(gs)
        currplayer match
          case Some(player) => 
            playersinfo + sep + player.name + " how many hands?"
          case None =>
            // - if a current player is not found we just return list of players info
            playersinfo
      case State.PlaceBets =>
        //define function that will return a list of the players
        //hands with the bet info
        def listhands(player : Player) : String =
          player.hands
                .zipWithIndex
                .map{ case(h, idx) => "Hand " + (idx.asInstanceOf[Int]+1) + "\t$" + h.bet }
                .mkString(sep)
        //list each players info followed by their hands
        val playersinfo = gs.players
                          .map(player =>
                               player.name + "\t$" + player.bankroll 
                               + "\tHands:" + player.hands.length + sep + listhands(player))
                          .mkString(sep)
        //find the current player that needs to place a bet
        //and prompt them to add to bet for their current hand
        val currplayer = findcurrplayer(gs)
        currplayer match 
          case Some(player) =>
            val betoptions = "[1] $5"+sep+"[2] $25"+sep+"[3] $50"+sep+"[4] $100"+sep+"[5] LOCK IN"+sep+"[6] CLEAR BET"+sep+"[7] DROP HAND"
            playersinfo + sep 
            + player.name + " add chips for hand " + (player.hands.indexWhere(hand => !hand.betplaced) + 1)
            + sep + betoptions
          case None =>
            playersinfo
      case State.Playing =>
        def myhands(player : Player) : String =
          player.hands.zipWithIndex
                      .map
                      { case(hand,idx) => 
                        "Hand " + (idx + 1) +"\tValue: " + handTotal(hand) + sep 
                        + handtoascii(hand) + "\tBet $" + hand.bet
                      }.mkString(sep+"---------------------------------"+sep)
        val seperator = sep+"#################################"+sep+"#################################"+sep
        val dealerinfo = "Dealer\tValue: " + handTotal(gs.dealer.drop(1)) + sep 
                         + handtoascii(gs.dealer.drop(1)) + seperator
        val playerinfo = gs.players.map(player => 
                                        player.name + "\t$" + player.bankroll + sep + myhands(player))
                                   .mkString(seperator)
        val currplayer = findcurrplayer(gs)
        currplayer match
          case Some(player) =>
            val currhandidx = player.hands.indexWhere(hand => !hand.stand)
            val currhand = player.hands(currhandidx)
            val playoptions = "[1] Hit"+sep+"[2] Stand"+sep
            val candouble = player.bankroll - currhand.bet >= 0  && currhand.hand.length == 2
            val cansplit = candouble && currhand.hand(0).value == currhand.hand(1).value
            val extraoptions = if candouble && cansplit then "[3] Double"+sep+"[4] Split"+sep
                               else if candouble then "[3] Double"+sep
                               else ""
            val prompt = player.name + " make a choice for hand " + (currhandidx + 1)
            dealerinfo + playerinfo + sep + prompt + sep + playoptions + extraoptions
          case None =>
            dealerinfo + playerinfo
      case State.Outcome =>
        def myhands(player : Player) : String =
          player.hands.zipWithIndex
                      .map
                      { case(hand,idx) => 
                        "Hand " + (idx + 1) +"\tValue: " + handTotal(hand) + sep 
                        + handtoascii(hand) + "\tBet $" + hand.bet + "\tPayout $" + hand.payout
                      }.mkString(sep+"---------------------------------"+sep)
        val seperator = sep+"#################################"+sep+"#################################"+sep
        val dealerinfo = "Dealer\tValue: " + handTotal(gs.dealer) + sep 
                         + handtoascii(gs.dealer) + seperator
        val playerinfo = gs.players.map(player => 
                                        player.name + "\t$" + player.bankroll + sep + myhands(player))
                                   .mkString(seperator)
        dealerinfo + playerinfo
      case State.Reshuffle =>
        "The deck has been reshuffled!!\nPress Enter To Continue..."
      case _ => ""


  def handtoascii(hand : Hand) : String =
    handtoascii(hand.hand)
  def handtoascii(cards : List[Cards.Card]) : String = 
    val asciicards = cards.map(card => cardtoascii(card))//.mkString
    val row1 = asciicards.map(card => card.split(sep)(0)).mkString
    val row2 = asciicards.map(card => card.split(sep)(1)).mkString
    val row3 = asciicards.map(card => card.split(sep)(2)).mkString
    val row4 = asciicards.map(card => card.split(sep)(3)).mkString
    val row5 = asciicards.map(card => card.split(sep)(4)).mkString
    val row6 = asciicards.map(card => card.split(sep)(5)).mkString
    row1 + sep + row2 + sep + row3 + sep + row4 + sep + row5 + sep + row6

  def cardtoascii(card : Cards.Card) : String =
    val row1 = " ------ "
    val row2 = "|" + card.facesym + "     |"
    val row3 = "|      |"
    val row4 = "|  " + card.suitsym + "  |"
    val row5 = "|      |"
    val row6 = "|_____" + card.facesym + "|"
    row1 + sep + row2 + sep + row3 + sep + row4 + sep + row5 + sep + row6 + sep
          

  def play_start(gs : GameState) : GameState =
    //read input for "press enter to continue"
    //set new state to selecthands
    readLine()
    GameState(State.SelectHands, gs.dealer, gs.players, gs.info)

  def play_selecthands(gs : GameState) : GameState =
    val currplayer = findcurrplayer(gs)
    currplayer match
      case Some(player) =>
        val input = try{ readInt() } catch { case e: NumberFormatException => -1 }
        input match
          case 0 =>
            val newplayers = gs.players diff List(player)
            GameState(State.SelectHands, gs.dealer, newplayers, gs.info)
          case handchoice if player.bankroll >= handchoice * gs.info.minbet =>
            val updatedplayer = selecthands(player, input)
            val newplayers = gs.players.updated(gs.players.indexOf(player),updatedplayer)
            GameState(State.SelectHands, gs.dealer, newplayers, gs.info)
          case _ => gs 
      case None => GameState(State.PlaceBets, gs.dealer, gs.players, gs.info)

  def play_placebets(gs : GameState) : GameState =
    val currplayer = findcurrplayer(gs)
    currplayer match
      case Some(player) =>
        val input = try{ readInt() } catch { case e: NumberFormatException => -1 }
        val updatedplayer = addtobet(player, gs.info, input)
        val updatedplayers = gs.players.updated(gs.players.indexOf(player),updatedplayer) 
        GameState(State.PlaceBets, gs.dealer, updatedplayers, gs.info)
      case None =>
        val validplayers = gs.players.filter(player => player.hands.length > 0)
        if validplayers.length > 0 then GameState(State.InitialDeal, gs.dealer, gs.players, gs.info) 
        else GameState(State.End, gs.dealer, gs.players, gs.info)

  def play_initialdeal(gs : GameState) : GameState =
    val totalhands = gs.players.map(player => player.hands.length).sum + 1
    val allhands = gs.players.map(player => player.hands).flatten:+gs.dealer
    val offset = gs.info.deckindex
    val allhandswithcards = allhands.zipWithIndex
                                    .map
                                    { case(hand,idx) =>
                                      val idx1 = idx + offset
                                      val idx2 = idx1 + totalhands
                                      List(gs.info.deck(idx1), gs.info.deck(idx2))
                                    }
    val playershands = allhandswithcards.dropRight(1)
    val playershandsgrouped = gs.players.zipWithIndex
                               .map
                               { case(player,idx) =>
                                 val startidx = if idx == 0 then 0 else gs.players(idx-1).hands.length
                                 val endidx = (player.hands.length) + startidx 
                                 playershands.slice(startidx,endidx)
                               }
    val newdealer = allhandswithcards.last
    val newplayers = gs.players
                       .zipWithIndex
                       .map
                       { case(player,idx) => 
                         val newhands = player
                                        .hands
                                        .zipWithIndex
                                        .map
                                        { case(hand,idxx) => 
                                          val newhand = playershandsgrouped(idx)(idxx)
                                          val bjvalue = hand.bet + (hand.bet * 3/2)
                                          val bjhand = Hand(newhand,hand.bet,hand.betplaced,true,bjvalue)
                                          val nonbjhand = Hand(newhand,hand.bet,hand.betplaced,hand.stand,hand.payout)
                                          if blackjack(newhand) then bjhand else nonbjhand
                                        }
                         Player(player.name, player.bankroll, newhands, player.handsplaying)
                       }
    val newinfo = GameInfo(gs.info.deck, (totalhands*2)+offset, gs.info.pen, gs.info.minbet, gs.info.maxbet)
    GameState(State.CheckDealerBJ, newdealer, newplayers, newinfo)

  def hithand(hand : Hand, info : GameInfo) : Hand =
    val newhand = hand.hand ++ List(info.deck(info.deckindex))
    val total = handTotal(newhand)
    total match
      case t if t > 21 =>
        Hand(newhand, hand.bet, hand.betplaced, true, hand.payout)
      case _ =>
        if hand.hand.length == 1 && hand.hand(0).facename == "Ace" then
          Hand(newhand, hand.bet, hand.betplaced, true, hand.payout)
        else
          Hand(newhand, hand.bet, hand.betplaced, false, hand.payout)

  def play_checkdealer(gs : GameState) : GameState =
    if blackjack(gs.dealer) then GameState(State.PlayDealer, gs.dealer, gs.players, gs.info)
    else GameState(State.Playing, gs.dealer, gs.players, gs.info)

  def play_playing(gs : GameState) : GameState =
    val currplayer = findcurrplayer(gs)
    currplayer match 
      case Some(player) =>
        val currhand = player.hands.filter(hand => !hand.stand).head
        if currhand.hand.length == 1 then 
            val newhand = hithand(currhand, gs.info)
            val newhands = player.hands.updated(player.hands.indexOf(currhand),newhand)
            val newplayer = Player(player.name, player.bankroll, newhands, player.handsplaying)
            val newplayers = gs.players.updated(gs.players.indexOf(player),newplayer)
            val newinfo = GameInfo(gs.info.deck, gs.info.deckindex + 1, gs.info.pen, gs.info.minbet, gs.info.maxbet)
            GameState(State.Playing, gs.dealer, newplayers, newinfo)
        else
            val input = try{ readInt() } catch{ case e: NumberFormatException => -1 }
            input match
              case 1 => //HIT
                val newhand = hithand(currhand, gs.info)
                val newhands = player.hands.updated(player.hands.indexOf(currhand),newhand)
                val newplayer = Player(player.name, player.bankroll, newhands, player.handsplaying)
                val newplayers = gs.players.updated(gs.players.indexOf(player),newplayer)
                val newinfo = GameInfo(gs.info.deck, gs.info.deckindex + 1, gs.info.pen, gs.info.minbet, gs.info.maxbet)
                GameState(State.Playing, gs.dealer, newplayers, newinfo)
              case 2 => //STAND
                val newhand = Hand(currhand.hand, currhand.bet, currhand.betplaced, true, currhand.payout)
                val newhands = player.hands.updated(player.hands.indexOf(currhand),newhand)
                val newplayer = Player(player.name, player.bankroll, newhands, player.handsplaying)
                val newplayers = gs.players.updated(gs.players.indexOf(player),newplayer)
                GameState(State.Playing, gs.dealer, newplayers, gs.info)
              case 3 => //DOUBLE
                val candouble = player.bankroll - currhand.bet >= 0 && currhand.hand.length == 2
                candouble match
                  case true => 
                    val handafterhit = hithand(currhand, gs.info)
                    val newhand = Hand(handafterhit.hand, handafterhit.bet * 2, handafterhit.betplaced, true, handafterhit.payout)
                    val newhands = player.hands.updated(player.hands.indexOf(currhand),newhand)
                    val newplayer = Player(player.name, player.bankroll - currhand.bet , newhands, player.handsplaying)
                    val newplayers = gs.players.updated(gs.players.indexOf(player),newplayer)
                    val newinfo = GameInfo(gs.info.deck, gs.info.deckindex + 1, gs.info.pen, gs.info.minbet, gs.info.maxbet)
                    GameState(State.Playing, gs.dealer, newplayers, newinfo)
                  case false => gs
              case 4 => //SPLIT
                val cansplit = player.bankroll - currhand.bet >= 0 
                               && currhand.hand.length == 2 
                               && currhand.hand(0).value == currhand.hand(1).value
                cansplit match
                  case true =>
                    val hand1 = List(currhand.hand(0))
                    val hand2 = List(currhand.hand(1))
                    val newhand1 = Hand(hand1, currhand.bet, currhand.betplaced, currhand.stand, currhand.payout)
                    val newhand2 = Hand(hand2, currhand.bet, currhand.betplaced, currhand.stand, currhand.payout)
                    val splitspot = player.hands.indexOf(currhand)
                    val handhalfs = player.hands.splitAt(splitspot)
                    val firsthalf = handhalfs(0)
                    val secondhalf = handhalfs(1).drop(1)
                    val newhands = firsthalf ++ List(newhand1,newhand2) ++ secondhalf
                    val newplayer = Player(player.name, player.bankroll - currhand.bet, newhands, player.handsplaying)
                    val newplayers = gs.players.updated(gs.players.indexOf(player),newplayer)
                    GameState(State.Playing, gs.dealer, newplayers, gs.info)
                  case false => gs
              case _ => gs
      case None =>
        GameState(State.PlayDealer, gs.dealer, gs.players, gs.info)

  def payouthands(hands : List[Hand], dealertotal : Int) : List[Hand] =
    val newhands = hands.map(hand =>
                              val total = handTotal(hand.hand)
                              val payout = total match
                                case t if hand.payout != 0 && dealertotal != 21 => hand.payout
                                case t if (t > dealertotal || dealertotal > 21) && t < 22 => hand.bet * 2
                                case t if t == dealertotal => hand.bet
                                case _ => 0
                              Hand(hand.hand, hand.bet, hand.betplaced, hand.stand, payout))
    newhands

  def play_playdealer(gs : GameState) : GameState = 
    val allbust = gs.players.filter(player => player.hands.forall(hand => handTotal(hand) > 21)).length == gs.players.length
    val allbj = gs.players.filter(player => player.hands.forall(hand => handTotal(hand) == 21)).length == gs.players.length
    handTotal(gs.dealer) match
      case total if total > 16 || allbust || allbj =>
        val playerhands = gs.players.map(player => player.hands)
        val newhands = playerhands.map(hands => payouthands(hands, total))
        val payouts = newhands.map(hands => hands.map(_.payout).sum)
        val newplayers = gs
                         .players
                         .zipWithIndex
                         .map
                         { case(player,idx) => 
                           Player(player.name, player.bankroll+payouts(idx), newhands(idx),player.handsplaying) 
                         }
        GameState(State.Outcome, gs.dealer, newplayers, gs.info)
      case _ =>
        val newdealer = gs.dealer ++ List(gs.info.deck(gs.info.deckindex))
        val newinfo = GameInfo(gs.info.deck, gs.info.deckindex + 1, gs.info.pen)
        GameState(State.PlayDealer, newdealer, gs.players, newinfo)

  def play_outcome(gs : GameState) : GameState =
    val newhandsplaying = gs.players
                            .map(player => 
                                if player.bankroll >= gs.info.minbet * player.handsplaying
                                   && player.hands.length >= player.handsplaying then player.handsplaying 
                                else if player.bankroll >= gs.info.minbet * player.hands.length then player.hands.length
                                else (player.bankroll/gs.info.minbet).asInstanceOf[Int]
                            )
    val players = gs.players.zipWithIndex
                            .map{ case(player,idx) => 
                                  val handsplaying = newhandsplaying(idx)
                                  Player(player.name, player.bankroll, List.fill(handsplaying)(Hand()), handsplaying) 
                                }
    val newplayers = players.filter(player => player.bankroll >= gs.info.minbet && player.hands.length > 0)
    readLine()
    gs.info.deckindex >= gs.info.pen match
      case true => 
        val newinfo = buildDoubleDeck
        GameState(State.Reshuffle, gs.dealer, newplayers, newinfo) 
      case false =>
        GameState(State.PlaceBets, gs.dealer, newplayers, gs.info)
  def play_reshuffle(gs : GameState) : GameState =
    readLine()
    val newplayers = gs.players.map(player => Player(player.name, player.bankroll, List(), 0))
    GameState(State.SelectHands, gs.dealer, newplayers, gs.info)

  @tailrec def play(gs : GameState) : Unit =
    /* for loop to 'clear screen' :( */
    for
      i <- 1 to 50
    do
      println("")
    println(play_output(gs))
    gs.state match
      case State.Start =>
        play(play_start(gs))
      case State.SelectHands =>
        play(play_selecthands(gs))
      case State.PlaceBets =>
        play(play_placebets(gs))
      case State.InitialDeal =>
        play(play_initialdeal(gs))
      case State.CheckDealerBJ =>
        play(play_checkdealer(gs))
      case State.Playing =>
        play(play_playing(gs))
      case State.PlayDealer =>
        play(play_playdealer(gs))
      case State.Outcome =>
        play(play_outcome(gs))
      case State.Reshuffle =>
        play(play_reshuffle(gs))
      case State.End => 

  /*
  	function that gets called to play the game.
	players are created and added to a player list.
	currently any number of players are supported although
	it is double deck... 
	(other decks can also be created see def buildDoubleDeck)

	players are defined by Player(name, bankroll)

	game info holds information about the game such as the deck, deck index,
		minimum bets, maximum bets.
	
	game is then created starting with a start state, an empty list representing the dealer,
		list of players, and the game's info.
	
	play function runs the game.
  */
  def play_blackjack() : Unit =
    val player = Player("Player",100.00) 
    val playerList = List(player)
    val gameinfo = buildDoubleDeck
    val game = GameState(State.Start, List(), playerList, gameinfo)
    play(game)
  
}
