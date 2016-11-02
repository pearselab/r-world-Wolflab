#' testing roxygen format for notes
#' hello R-world
#' ok - let's go
#' 

# Overall plan
# 1: Make a square matrix with odd dimensions (user defined - check for being odd and a square)
# 2: Pick starting heights for four corners (rnorm?)
# 3: repeat for all squares that need to be filled:
#   4: DIAMOND-STEP(matrix)
#     4.1: find mid-point (divide by two - check)
#     4.2: make that mid-point an average of corners - add noise (jitter?)
#   5: SQUARE-STEP(matrix)
#     5.1: find mid points of all edges
#     5.2: make that mid-point an average of adjacent corners and center - add noise (jitter?)
#     5.3: *** tricky*** if this is an internal square, do I need to include CEDNTER from another
#          square (hard to explain. See if Will understands what I mean)
# 6: until Matrix filled with values
# 7. Convert matrix to terrain using heat map, blue for below zero