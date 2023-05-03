pacman::p_load(covr)

covr = file_coverage('functions.R', 'test_functions.R')
report(covr)
