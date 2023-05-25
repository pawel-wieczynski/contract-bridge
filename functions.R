get_figures = function(){
  
  figures = c(2:10, 'J', 'Q', 'K', 'A')
  figures = factor(figures, ordered = TRUE, levels = figures)
  
  return(figures)
  
}

get_colors = function(with_nt = FALSE, icons = FALSE){
  
  if (icons) {
    colors = c('♣', '♦', '♥', '♠')
  } else {
    colors = c('clubs', 'diamonds', 'hearts', 'spades')
  }
  
  if (with_nt) colors = c(colors, 'NT')
  colors = factor(colors, ordered = TRUE, levels = colors)
  
  return(colors)
  
}

suit_to_icon = function(suit) {
  
  suit_icons = c(
    'clubs' = '♣'
    , 'diamonds' = '♦'
    , 'hearts' = '♥'
    , 'spades' = '♠'
    , 'NT' = 'NT')
  
  return(suit_icons[suit])
  
}

icon_to_suit = function(icon) {
  
  icon_suits = c(
    '♣' = 'clubs'
    , '♦' = 'diamonds'
    , '♥' = 'hearts'
    , '♠' = 'spades'
    , 'NT' = 'NT')
  
  return(icon_suits[icon])
  
}

replace_icon = function(text){
  
  text = sub('♣', ' clubs', text)
  text = sub('♦', ' diamonds', text)
  text = sub('♥', ' hearts', text)
  text = sub('♠', ' spades', text)
  
  return(text)
  
}

get_all_bids = function(){
  
  bids = expand.grid(
    colors = get_colors(with_nt = TRUE, icons = TRUE)
    ,levels = factor(1:7, ordered = TRUE)
  )
  
  bids = c(paste0(bids$levels, bids$colors), 'PASS')
  
  return(bids)
  
}

get_possible_bids = function(level = 1, color = 'clubs') {
  # function for eliminating already used bids
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

set_dealer = function() {
  dealer  = sample(c('north', 'east', 'south', 'west'), 1)
  return(dealer)
}

print_hand = function(hand){
  
  # TBD functionalize below as well

  spades = paste0('♠ ', paste(sort(hand[hand$colors == 'spades', 1], decreasing = TRUE), collapse = ''))
  hearts = paste0('♥ ', paste(sort(hand[hand$colors == 'hearts', 1], decreasing = TRUE), collapse = ''))
  diamonds = paste0('♦ ', paste(sort(hand[hand$colors == 'diamonds', 1], decreasing = TRUE), collapse = ''))
  clubs = paste0('♣ ', paste(sort(hand[hand$colors == 'clubs', 1], decreasing = TRUE), collapse = ''))
  
  hand_string = paste(spades, hearts, diamonds, clubs, sep = '\n')
  
  return(hand_string)
  
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
  return(c(spades = spades, hearts = hearts, diamonds = diamonds, clubs = clubs))
}

next_player = function(player) {
  
  next_player_ = switch(
    player
    ,'north' = 'east'
    ,'east' = 'south'
    ,'south' = 'west'
    ,'west' = 'north'
  )
  
  return(next_player_)
  
}