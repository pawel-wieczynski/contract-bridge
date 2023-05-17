if (!require('testthat')) install.packages('testthat')
library(testthat)
source('functions.R')

# TBD: test if is ordered factor
test_that('Correct data types', {
  expect_s3_class(get_figures(), 'factor')
  expect_s3_class(get_colors(), 'factor')
  expect_s3_class(set_trump('spades'), 'factor')
  expect_type(next_player('north'), 'character')
  expect_type(set_dealer(), 'character')
  
  # List is neither S3 nor S4 object
  expect_s3_class(shuffle_cards(), NA)
  expect_s4_class(shuffle_cards(), NA)
  expect_s3_class(shuffle_cards()[[1]], 'data.frame')
  expect_s3_class(shuffle_cards()[[2]], 'data.frame')
  expect_s3_class(shuffle_cards()[[3]], 'data.frame')
  expect_s3_class(shuffle_cards()[[4]], 'data.frame')

})

test_that('Correct output size', {
  expect_equal(length(get_figures()), 13)
  expect_equal(length(get_colors()), 4)
  expect_equal(length(get_colors(with_nt = TRUE)), 5)
  expect_equal(length(set_trump('hearts')), 4)
  expect_equal(length(shuffle_cards()), 4)
})

test_that('suit_to_icon works as expected', {
  expect_equal(unname(suit_to_icon('hearts')), '♥')
  expect_equal(unname(suit_to_icon('clubs')), '♣')
  expect_equal(unname(suit_to_icon('diamonds')), '♦')
  expect_equal(unname(suit_to_icon('spades')), '♠')
  expect_equal(unname(suit_to_icon('NT')), 'NT')
  expect_error(unname(suit_to_icon('abc')), NA)
})

test_that('icon_to_suit works as expected', {
  expect_equal(unname(icon_to_suit('♥')), 'hearts')
  expect_equal(unname(icon_to_suit('♣')), 'clubs')
  expect_equal(unname(icon_to_suit('♦')), 'diamonds')
  expect_equal(unname(icon_to_suit('♠')), 'spades')
  expect_equal(unname(icon_to_suit('NT')), 'NT')
  expect_error(unname(icon_to_suit('abc')), NA)
})

test_that('Trump is set correctly', {
  expect_equal(levels(set_trump('clubs'))[4], 'clubs')
  expect_equal(levels(set_trump('diamonds'))[4], 'diamonds')
  expect_equal(levels(set_trump('hearts'))[4], 'hearts')
  expect_equal(levels(set_trump('spades'))[4], 'spades')
})

test_that('Total amount of HCP is 40', {
  expect_equal(sum(sapply(shuffle_cards(), calculate_hcp)), 40)
})

test_that('After shuffle number of cards in each suit is 13', {
  expect_equal(sum(rowSums(sapply(shuffle_cards(), count_suits)) == 13), 4)
})

test_that('After shuffle every player has 13 cards', {
  expect_equal(sum(colSums(sapply(shuffle_cards(), count_suits)) == 13), 4)
})

test_that('Correct sequence of players.', {
  expect_equal(next_player('north'), 'east')
  expect_equal(next_player('east'), 'south')
  expect_equal(next_player('south'), 'west')
  expect_equal(next_player('west'), 'north')
})

