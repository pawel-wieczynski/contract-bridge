if(!require('pacman')) install.packages('pacman')
pacman::p_load(svDialogs)
source('bridge.R')

possible_contracts = expand.grid(
  levels = factor(1:2, ordered = TRUE)
  ,colors = get_colors(with_nt = TRUE)
)

possible_contracts = c(paste0(possible_contracts$levels, ' ', possible_contracts$colors), 'PASS')

players = shuffle_cards()
print_hand(players$south)

opening = svDialogs::dlg_list(possible_contracts, title = 'How would you open?')$res

opening_correct = open_bid(players$south)

if (opening == opening_correct) {
  cat('\n\nCorrect.')
} else {
  cat(paste0('\n\nIncorrect. Correct opening should be ', opening_correct, '.'))
}
