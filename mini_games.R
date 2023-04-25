if(!require('pacman')) install.packages('pacman')
pacman::p_load(svDialogs)
source('bridge.R')

possible_contracts = expand.grid(
  figures = c(1:2)
  ,colors = c('spades', 'hearts', 'diamonds', 'clubs', 'NT')
)

possible_contracts = c(sort(paste0(possible_contracts$figures, ' ', possible_contracts$colors)), 'PASS')

players = shuffle_cards()
print_hand(players$south)

opening = svDialogs::dlg_list(possible_contracts, title = 'How would you open?')$res

opening_correct = open_bid(players$south)

if (opening == opening_correct) {
  cat('\n\nCorrect.')
} else {
  cat(paste0('\n\nIncorrect. Correct opening should be ', opening_correct, '.'))
}
