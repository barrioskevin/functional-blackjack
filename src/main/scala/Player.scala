class Player(
  val name : String,
  val bankroll : Double,
  val hands : List[List[Cards.Card]] = List(),
  val bet : Double = 0)
end Player
