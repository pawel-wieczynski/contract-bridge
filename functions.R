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

# Openings
open_bid = function(hand){
  
  hcp = calculate_hcp(hand)
  suits = count_suits(hand)
  
  # 1st level openings ----
  if (suits['spades'] >= 5 & hcp >= 12 & hcp <= 18) {
    return('1 spades')
  } else if (suits['hearts'] >= 5 & hcp >= 12 & hcp <= 18) {
    return('1 hearts')
  } else if (suits['diamonds'] >= 5 & hcp >= 12 & hcp <= 18) {
    return('1 diamonds')
  } else if (suits['clubs'] >= 2 & hcp >= 12 & hcp <= 18) {
    return('1 clubs')
  } else if (suits['spades'] >= 2 & suits['spades'] <= 4 & 
             suits['hearts'] >= 2 & suits['hearts'] <= 4 & 
             suits['diamonds'] >= 2 & suits['diamonds'] <= 4 &
             suits['clubs'] >= 2 & suits['clubs'] <= 4 &
             hcp >= 16 & hcp <= 18) {
    return('1 NT')
    
  # 2nd level openings ----
  } else if (suits['spades'] >= 5 & hcp >= 19 & hcp <= 22) {
    return('2 spades')
  } else if (suits['hearts'] >= 5 & hcp >= 19 & hcp <= 22) {
    return('2 hearts')
  } else if (suits['diamonds'] >= 4 & hcp >= 19 & hcp <= 22) {
    return('2 diamonds')
  } else if (suits['clubs'] >= 4 & hcp >= 19 & hcp <= 22) {
    return('2 clubs')
  } else if (suits['spades'] >= 2 & suits['spades'] <= 4 & 
             suits['hearts'] >= 2 & suits['hearts'] <= 4 & 
             suits['diamonds'] >= 2 & suits['diamonds'] <= 4 &
             suits['clubs'] >= 2 & suits['clubs'] <= 4 &
             hcp >= 19 & hcp <= 22) {
    return('2 NT')
    
  # 3rd level openings ----
  } else if (suits['spades'] >= 7 & hcp >= 5 & hcp <= 7) {
    return('3 spades')
  } else if (suits['hearts'] >= 7 & hcp >= 5 & hcp <= 7) {
    return('3 hearts')
  } else if (suits['diamonds'] >= 7 & hcp >= 5 & hcp <= 7) {
    return('3 diamonds')
  } else if (suits['clubs'] >= 7 & hcp >= 5 & hcp <= 7) {
    return('3 clubs')
    
  } else {
    return('PASS')
  }
  
}

answer_after_open = function(hand, open) {
  
  hcp = calculate_hcp(hand)
  suits = count_suits(hand)
  
  if (open == '1 spades') {
    if (hcp >= 7 & suits['spades'] >= 3) {
      return('2 spades')
    } else if (hcp >= 13 & suits['spades'] >= 3) {
      return('spades')
    }
  }
  
  else if (open == '1 hearts') {
    if (hcp >= 7 & suits['hearts'] >= 3) {
      return('2 hearts')
    } else if (hcp >= 13 & suits['hearts'] >= 3) {
      return('hearts')
    }
  }
  
  else if (open == '1 diamonds') {
    if (hcp >= 7 & suits['diamonds'] >= 3) {
      return('2 diamonds')
    } else if (hcp >= 13 & suits['diamonds'] >= 3) {
      return('diamonds')
    }
  }
  
  else if (open == '1 clubs') {
    if (hcp >= 7 & suits['clubs'] >= 3) {
      return('2 clubs')
    } else if (hcp >= 13 & suits['clubs'] >= 3) {
      return('clubs')
    }
  }
  
}

# players = shuffle_cards()
# sapply(players, count_suits)
# sapply(players, calculate_hcp)
# sapply(players, open_bid)









