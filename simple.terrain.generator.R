#set matrix dimension
n <- 10

#create matrix
tm <- matrix(0, n, n)

#set tm[1,1] elevation
tm[1,1] <- rnorm(1, 1000, 500) 

# set values for remaining points in first row.
# each value is function of value already created to left
for(col in 2:n){
  tm[1,col] <- rnorm(1, tm[1, col-1], abs(tm[1, col-1]))
}

# do same for remaining cells. But now each cell is rnorm function of mean of value to left and above
for(row in 2:n){
  for(col in 1:n){
    if(col == 1){
      tm[row,1] <- (rnorm(1, tm[row-1, 1], abs(tm[row-1, 1])))
    }else{
      tm[row,col] <- rnorm(1, mean(tm[row-1, col], tm[row, col-1]), mean(abs(tm[row-1, col]), abs(tm[row, col-1])))
    }
  }
}
print(tm)
