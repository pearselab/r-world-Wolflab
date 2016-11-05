#' testing roxygen format for notes
#' hello R-world
#' ok - let's go
#' 

# Overall plan
# 1: Make a square matrix with odd dimensions (user defined - check for being odd and a square)
#       Note that I now build this a function of how many 'cycles' of reduction is requested.
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
# 7: Run the diamond.square.step to cycle through levels and complete matrix
# 7. Convert matrix to terrain using heat map, blue for below zero

#set matrix size
# start with number of reduction cycles:
numb.matrix.rows <- function(n, rows=3){
  if(n > 17){
    return("too many cycles, please enter smaller number")# This certainly prevents making a matrix
            # that is too big - but it should give a better error message at the approporiate spot
  }else{
  for(i in 1:n){
    rows <- rows + (rows-1)
  }
  }
  return(rows)
}
# now make matrix numb.matrix.rows x same
n <- 2 # number of reduction cycles

#print(numb.matrix.rows(n))
terrain.matrix <- matrix(0, numb.matrix.rows(n), numb.matrix.rows(n))
#print(nrow(terrain.matrix))
#print(terrain.matrix)
total.dim <- numb.matrix.rows(n)
print(total.dim)
initiate.landscape.matrix <- function(terrain.matrix){
  max.rows <- nrow(terrain.matrix)# find number of rows
  #use random number generator to set the corners
  terrain.matrix[1,1] <- rnorm(1, 1000, 1000)
  terrain.matrix[1,max.rows] <- rnorm(1, 1000, 1000)
  terrain.matrix[max.rows,1] <- rnorm(1, 1000, 1000)
  terrain.matrix[max.rows,max.rows] <- rnorm(1, 1000, 1000)
  return(terrain.matrix)
}
terrain.matrix <- initiate.landscape.matrix(terrain.matrix) # corner values added
#print(terrain.matrix)


diamond.step <- function(tm){
  max.rows <- nrow(terrain.matrix)# find number of rows. Assume that this function can be applied to matrix subset
  mid.point <- (max.rows+1)/2# find center point
  tm[mid.point,mid.point] <- mean(c(tm[1,1], tm[1,max.rows], tm[max.rows,1], tm[max.rows,max.rows]))
  # Above line just finds the center of matrix and makes it mean of 4 corner values
  return(tm)
}
terrain.matrix <- diamond.step(terrain.matrix)
#print(terrain.matrix)

#now a function to check if a cell is valid
is.valid.cell <- function(pot.rows, pot.cols, total.dim){
  if(pot.rows > 0 & pot.rows <= total.dim & pot.cols > 0 & pot.cols <= total.dim){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

print(is.valid.cell(10,9,9))

valid.cells <- function(pot.rows, pot.cols, total.dim){
  good.rows <- pot.rows > 0 & pot.rows <= nrow
  pot.rows <- pot.rows[good.rows]
  pot.cols <- pot.cols[good.rows]
  #...the same thing for columns...
  #...some sort of return statement
}
first.square.step <- function(tm){
  max.rows <- nrow(tm)# find number of rows
  mid.point <- (max.rows+1)/2# find center point
  tm[1,mid.point] <- mean(c(tm[1,1], tm[1,max.rows], tm[mid.point, mid.point]))# get top midpoint
  tm[mid.point,1] <- mean(c(tm[1,1], tm[max.rows,1], tm[mid.point, mid.point]))# get left midpoint
  tm[mid.point,max.rows] <- mean(c(tm[1, max.rows], tm[max.rows,max.rows], tm[mid.point, mid.point]))# get right midpoint
  tm[max.rows,mid.point] <- mean(c(tm[max.rows,1], tm[max.rows,max.rows], tm[mid.point, mid.point]))# get bottom midpoint
  return(tm) 
}

general.square.step <- function(terrain.matrix, tm){
  total.dim <- nrow(terrain.matrix)
  max.rows <- nrow(tm)# find number of rows in this square
  mid.point <- (max.rows+1)/2# find center point in this square
  top.midpoint <- c(tm[1,1], tm[1,max.rows], tm[mid.point, mid.point])
  if(is.valid.cell(mid.point, mid.point + 3, total.dim))
  tm[1,mid.point] <- mean(c(tm[1,1], tm[1,max.rows], tm[mid.point, mid.point]))# get top midpoint
  tm[mid.point,1] <- mean(c(tm[1,1], tm[max.rows,1], tm[mid.point, mid.point]))# get left midpoint
  tm[mid.point,max.rows] <- mean(c(tm[1, max.rows], tm[max.rows,max.rows], tm[mid.point, mid.point]))# get right midpoint
  tm[max.rows,mid.point] <- mean(c(tm[max.rows,1], tm[max.rows,max.rows], tm[mid.point, mid.point]))# get bottom midpoint
  return(tm) 
}

terrain.matrix <- first.square.step(terrain.matrix) 
print(terrain.matrix)

#mini.matrix <- terrain.matrix[1:5, 1:5]
#print(general.square.step(terrain.matrix, mini.matrix, total.dim))
#print(terrain.matrix[1-3,max.rows])
#cycle.thru.landscape <- function(matrix.so.far)
