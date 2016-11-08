#vector arithmetic
# Let's make sure I understand that "numeric" is used to INITIATE and empty vector of length
# (single arguemnt required)
# Whereas "c" means concantenate all arguments to be the actual values in the vector

# This example show first how to fill a vector the long-winded loop method
# Followed by the simpelr vector arithmetic approach

paul.sq.long.winded <- function(numb.elements){
  sq.vect <- numeric(numb.elements)
  for(i in 1:numb.elements){
    sq.vect[i] <- i^2
  }
  return(sq.vect)
}



paul.square <- function(numb.elements){
  sq.vect <- (1:numb.elements)^2
  return(sq.vect)
}
print(paul.square(15))

print(paul.sq.long.winded(7))


