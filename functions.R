get_figures = function(){
  
  figures = c(2:10, 'J', 'Q', 'K', 'A')
  figures = factor(figures, ordered = TRUE, levels = figures)
  
  return(figures)
  
}

get_colors = function(with_nt = FALSE){
  
  colors = c('clubs', 'diamonds', 'hearts', 'spades')
  if (with_nt) colors = c(colors, 'NT')
  colors = factor(colors, ordered = TRUE, levels = colors)
  
  return(colors)
  
}

set_trump = function(trump){
  
  colors = get_colors()
  no_trump_colors = setdiff(colors, trump)
  colors = factor(colors, ordered = TRUE, levels = c(no_trump_colors, trump))
  
  return(colors)
  
}

shuffle_cards = function(){
  
  deck = expand.grid(
    figures = get_figures()
    ,colors = get_colors()
  )
  
  deck$cards = 1:52
  
  players = list(
    north = c()
    ,east = c()
    ,south = c()
    ,west = c()
  )
  
  # TBD simplify
  for (i in seq_along(players)){
    players[[i]] = deck[deck$cards %in% sample(deck$cards, 13, replace = FALSE), ]
    deck = deck[deck$cards %in% setdiff(deck$cards, players[[i]]$cards), ]
  }
  
  return(players)
  
}

print_hand = function(hand){
  
  # TBD functionalize below as well
  # TBD sorting based on cards ordinality
  spades = paste0('Spades: ', paste(sort(hand[hand$colors == 'spades', 1], decreasing = TRUE), collapse = ', '))
  hearts = paste0('Hearts: ', paste(sort(hand[hand$colors == 'hearts', 1], decreasing = TRUE), collapse = ', '))
  diamonds = paste0('Diamonds: ', paste(sort(hand[hand$colors == 'diamonds', 1], decreasing = TRUE), collapse = ', '))
  clubs = paste0('Clubs: ', paste(sort(hand[hand$colors == 'clubs', 1], decreasing = TRUE), collapse = ', '))
  
  cat(paste(spades, hearts, diamonds, clubs, sep = '\n'))
  
}

calculate_hcp = function(hand) {
  figures = hand$figures
  hcp = 1 * length(grep('J', figures)) + 2 * length(grep('Q', figures)) + 3 * length(grep('K', figures)) + 4 * length(grep('A', figures))
  return(hcp)
}

count_suits = function(hand) {
  colors = hand$colors
  spades = length(grep('spades', colors))
  hearts = length(grep('hearts', colors))
  diamonds = length(grep('diamonds', colors))
  clubs = length(grep('clubs', colors))
  return(list(spades = spades, hearts = hearts, diamonds = diamonds, clubs = clubs))
}

# sapply(players, count_suits)
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
  } else if (suits$spades >= 2 & suits$spades <= 4 & 
             suits$hearts >= 2 & suits$hearts <= 4 & 
             suits$diamonds >= 2 & suits$diamonds <= 4 &
             suits$clubs >= 2 & suits$clubs <= 4 &
             hcp >= 16 & hcp <= 18) {
    return('1 NT')
    
  } else if (suits$spades >= 5 & hcp >= 19 & hcp <= 22) {
    return('2 spades')
  } else if (suits$hearts >= 5 & hcp >= 19 & hcp <= 22) {
    return('2 hearts')
  } else if (suits$diamonds >= 4 & hcp >= 19 & hcp <= 22) {
    return('2 diamonds')
  } else if (suits$clubs >= 4 & hcp >= 19 & hcp <= 22) {
    return('2 clubs')
  } else if (suits$spades >= 2 & suits$spades <= 4 & 
             suits$hearts >= 2 & suits$hearts <= 4 & 
             suits$diamonds >= 2 & suits$diamonds <= 4 &
             suits$clubs >= 2 & suits$clubs <= 4 &
             hcp >= 19 & hcp <= 22) {
    return('2 NT')
  } else {
    return('PASS')
  }
  
}

players = shuffle_cards()
sapply(players, count_suits)
sapply(players, calculate_hcp)
sapply(players, open_bid)










