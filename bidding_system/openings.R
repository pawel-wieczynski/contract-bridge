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