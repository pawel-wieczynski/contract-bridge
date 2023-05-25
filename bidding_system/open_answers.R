open_answers = function(hand, open) {
  
  hcp = calculate_hcp(hand)
  suits = count_suits(hand)
  
  # Answers after 1st level open ----
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
  
  # Answers after 2nd level open ----
  else if (open == '2 spades') {
    if (hcp >= 6 & suits['spades'] >= 3) {
      return('3 spades')
    } else if (hcp >= 3 & suits['spades'] >= 3) {
      return('4 spades')
    }
  }
  
  else if (open == '2 hearts') {
    if (hcp >= 6 & suits['hearts'] >= 3) {
      return('3 hearts')
    } else if (hcp >= 3 & suits['hearts'] >= 3) {
      return('4 hearts')
    }
  }
  
  else if (open == '2 diamonds') {
    if (hcp >= 6 & suits['diamonds'] >= 3) {
      return('3 diamonds')
    } else if (hcp >= 3 & suits['diamonds'] >= 3) {
      return('4 diamonds')
    }
  }
  
  else if (open == '2 clubs') {
    if (hcp >= 6 & suits['clubs'] >= 3) {
      return('3 clubs')
    } else if (hcp >= 3 & suits['clubs'] >= 3) {
      return('4 clubs')
    }
  }
  
  # When partner's suit is not fitting ----
  for (color in c('spades', 'hearts', 'diamonds', 'clubs')) {
    if (suits[color] >= 4) {
      if (substr(open, 1, 2) == '1' & hcp >= 7) {
        return(paste('2', color))
      } else if (substr(open, 1, 2) == '1' & hcp >= 13) {
        return(paste('3', color))
      } else if (substr(open, 1, 2) == '2' & hcp >= 6) {
        return(paste('3', color))
      } else if (substr(open, 1, 2) == '2' & hcp >= 3) {
        return(paste('4', color))
      }
    }
  }
  
}