deck = expand.grid(
  c(2:10, 'J', 'Q', 'K', 'A')
  ,c('spades', 'hearts', 'diamonds', 'clubs')
)

deck = paste0(deck$Var1, ' (', deck$Var2, ')')

players = list(
  north = c()
  ,east = c()
  ,south = c()
  ,west = c()
)

for (i in seq_along(players)) {
  players[[i]] = sample(deck, 13, replace = FALSE)
  deck = setdiff(deck, players[[i]])
}

calculate_hcp = function(hand) {
  hcp = 1 * length(grep('J', hand)) + 2 * length(grep('Q', hand)) + 3 * length(grep('K', hand)) + 4 * length(grep('A', hand))
  return(hcp)
}

sapply(players, calculate_hcp)

count_suits = function(hand) {
  spades = length(grep('spades', hand))
  hearts = length(grep('hearts', hand))
  diamonds = length(grep('diamonds', hand))
  clubs = length(grep('clubs', hand))
  return(list(spades = spades, hearts = hearts, diamonds = diamonds, clubs = clubs))
}

sapply(players, count_suits)
#test: rowSums = 13, colSums = 13

# Openings
open_bid = function(hand){
  
  hcp = calculate_hcp(hand)
  suits = count_suits(hand)
  
  if (suits$spades >= 5 & hcp >= 12 & hcp <= 18) {
    return('1 spades')
  } else if (suits$hearts >= 5 & hcp >= 12 & hcp <= 18) {
    return('1 hearts')
  } else if (suits$diamonds >= 5 & hcp >= 12 & hcp <= 18) {
    return('1 diamonds')
  } else if (suits$clubs >= 2 & hcp >= 12 & hcp <= 18) {
    return('1 clubs')
    
  } else if (suits$spades >= 5 & hcp >= 19 & hcp <= 22) {
    return('2 spades')
  } else if (suits$hearts >= 5 & hcp >= 19 & hcp <= 22) {
    return('2 hearts')
  } else if (suits$diamonds >= 4 & hcp >= 19 & hcp <= 22) {
    return('2 diamonds')
  } else if (suits$clubs >= 4 & hcp >= 19 & hcp <= 22) {
    return('2 clubs')
  }
  
}

sapply(players, open_bid)









