#' testing roxygen format for notes
#' hello R-world
#' ok - let's go
#' 

# Overall plan
# 1: Make a square matrix with odd dimensions (user defined - check for being odd and a square)
# 1.1 hold your horses. That is not so easy as it sounds. It must be more than odd, it must remain
#      odd all the way down the hiearchy of squares. So it must need to be 3 to the power of 
#      something so check that a) ncol == nrow  AND b) for (i in 1:10): (ncol - 3^1) == 0
# 2: Pick starting heights for four corners (rnorm?)
# 3: repeat for all squares that need to be filled:
#   4: DIAMOND-STEP(matrix)
#     4.1: find mid-point (divide by two - check)
#     4.2: make that mid-point an average of corners - add noise (jitter?)
#     4.3: feed that value back to main landscape matrix
#     4.4: Do this for ALL sub-squared (important)
#   5: SQUARE-STEP(matrix)
#     5.1: find mid points of all edges
#     5.2: make that mid-point an average of adjacent corners and centers - add noise (jitter?)
#     5.3: *** tricky*** if this is an internal square, do I need to include CENTER from another
#          square (hard to explain. Must refer to full landscape matrix)
#     5.4: feed that value back to main landscape matrix
#     5.5: Do this for all sub-squares
# 6: until Matrix filled with values
# 7. Convert matrix to terrain using heat map, blue for below zero

#set matrix size
# start with number of reduction cycles:
numb.matrix.rows <- function(n, rows=3){
  for(i in 1:n){
    rows <- rows + (rows-1)
  }
  return(rows)
}
print(numb.matrix.rows(3))
# now make matrix numb.matrix.rows x same
terrain.matrix <- matrix(0, numb.matrix.rows(3), numb.matrix.rows(3))
print(terrain.matrix)