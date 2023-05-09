source('functions.R')

bidding = TRUE
players = shuffle_cards()
dealer = set_dealer()

while (bidding) {
  
  # Dealer opens
  player = dealer
  opening_1 = open_bid(players[[player]])
  
  # Opponent opens
  player = next_player(player)
  opening_2 = open_bid(players[player])
  
  # Dealers partner
  player = next_player(player)
  if (opening_1 == 'PASS' & opening_2 == 'pass') {
    answer_1 = open_bid(players[[player]])
  } else {
    answer_1 = answer_after_open(players[player], opening_1)
  }
  
}
