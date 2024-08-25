object Cards
{
  enum Face:
    case Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace
  enum Suit:
    case Spade,Heart,Club,Diamond
  case class Card(face : Face, suit : Suit):
    val facename = face match
        case Face.Two => "Two"
        case Face.Three => "Three"
        case Face.Four => "Four"
        case Face.Five => "Five"
        case Face.Six => "Six"
        case Face.Seven => "Seven"
        case Face.Eight => "Eight"
        case Face.Nine => "Nine"
        case Face.Ten => "Ten"
        case Face.Jack => "Jack"
        case Face.Queen => "Queen"
        case Face.King => "King"
        case Face.Ace => "Ace"
    val suitname = suit match
        case Suit.Spade => "Spade"
        case Suit.Heart => "Heart"
        case Suit.Club => "Club"
        case Suit.Diamond => "Diamond"
    val facesym = face match
        case Face.Two => "2"
        case Face.Three => "3"
        case Face.Four => "4"
        case Face.Five => "5"
        case Face.Six => "6"
        case Face.Seven => "7"
        case Face.Eight => "8"
        case Face.Nine => "9"
        case Face.Ten => "T"
        case Face.Jack => "J"
        case Face.Queen => "Q"
        case Face.King => "K"
        case Face.Ace => "A"
    val suitsym = suit match
        case Suit.Spade => "{>"
        case Suit.Heart => "<3"
        case Suit.Club => "-%"
        case Suit.Diamond => "<>"
    val name = facename + " of " + suitname + "s"
    val value : Int = face match
      case Face.Two => 2 
      case Face.Three => 3 
      case Face.Four => 4 
      case Face.Five => 5 
      case Face.Six => 6 
      case Face.Seven => 7 
      case Face.Eight => 8 
      case Face.Nine => 9 
      case Face.Ten => 10 
      case Face.Jack => 10 
      case Face.Queen => 10 
      case Face.King => 10 
      case Face.Ace => 11 
    override def toString = name
  end Card

  val deck = List(Card(Face.Ace, Suit.Spade), //0
                  Card(Face.Two, Suit.Spade), //1
                  Card(Face.Three, Suit.Spade), //2
                  Card(Face.Four, Suit.Spade), //3
                  Card(Face.Five, Suit.Spade), //4
                  Card(Face.Six, Suit.Spade), //5
                  Card(Face.Seven, Suit.Spade), //6
                  Card(Face.Eight, Suit.Spade), //7
                  Card(Face.Nine, Suit.Spade), //8
                  Card(Face.Ten, Suit.Spade), //9
                  Card(Face.Jack, Suit.Spade), //10
                  Card(Face.Queen, Suit.Spade), //11
                  Card(Face.King, Suit.Spade), //12
                  Card(Face.Ace, Suit.Heart), //13
                  Card(Face.Two, Suit.Heart), //14
                  Card(Face.Three, Suit.Heart), //15
                  Card(Face.Four, Suit.Heart), //16
                  Card(Face.Five, Suit.Heart), //17
                  Card(Face.Six, Suit.Heart), //18
                  Card(Face.Seven, Suit.Heart), //19
                  Card(Face.Eight, Suit.Heart), //20
                  Card(Face.Nine, Suit.Heart), //21
                  Card(Face.Ten, Suit.Heart), //22
                  Card(Face.Jack, Suit.Heart), //23
                  Card(Face.Queen, Suit.Heart), //24
                  Card(Face.King, Suit.Heart), //25
                  Card(Face.Ace, Suit.Club), //26
                  Card(Face.Two, Suit.Club), //27
                  Card(Face.Three, Suit.Club), //28
                  Card(Face.Four, Suit.Club), //29
                  Card(Face.Five, Suit.Club), //30
                  Card(Face.Six, Suit.Club), //31
                  Card(Face.Seven, Suit.Club), //32
                  Card(Face.Eight, Suit.Club), //33
                  Card(Face.Nine, Suit.Club), //34
                  Card(Face.Ten, Suit.Club), //35
                  Card(Face.Jack, Suit.Club), //36
                  Card(Face.Queen, Suit.Club), //37
                  Card(Face.King, Suit.Club), //38
                  Card(Face.Ace, Suit.Diamond), //39
                  Card(Face.Two, Suit.Diamond), //40
                  Card(Face.Three, Suit.Diamond), //41
                  Card(Face.Four, Suit.Diamond), //42
                  Card(Face.Five, Suit.Diamond), //43
                  Card(Face.Six, Suit.Diamond), //44
                  Card(Face.Seven, Suit.Diamond), //45
                  Card(Face.Eight, Suit.Diamond), //46
                  Card(Face.Nine, Suit.Diamond), //47
                  Card(Face.Ten, Suit.Diamond), //48
                  Card(Face.Jack, Suit.Diamond), //49
                  Card(Face.Queen, Suit.Diamond), //50
                  Card(Face.King, Suit.Diamond)) //51
}
