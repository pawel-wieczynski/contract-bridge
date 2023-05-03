if (!require('testthat')) install.packages('testthat')
library(testthat)
source('functions.R')

# TBD: test if is ordered factor
test_that('Correct data types', {
  expect_s3_class(get_figures(), 'factor')
  expect_s3_class(get_colors(), 'factor')
})

test_that('Correct output size', {
  expect_equal(length(get_figures()), 13)
  expect_equal(length(get_colors()), 4)
  expect_equal(length(get_colors(with_nt = TRUE)), 5)
})
