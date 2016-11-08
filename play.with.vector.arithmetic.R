#vector arithmetic
# Let's make sure I understand that "numeric" is used to INITIATE and\ empty vector of length n
# (single arguemnt required)
# Whereas "c" means concantenate all arguments to be the actual values in the vector

# This example show first how to fill a vector the long-winded loop method
# Followed by the simpler vector arithmetic approach
# This seems to work if I want to do everything to every element of a vector
# But cannot get approach to work if a) subsetting  B) conditionally apply function (will need an if
# statement somewhere) or c) the new vector elements are a function of other elements. In this case
# I either need a loop, or I must figure out how to do recursive function for vector, which does
# not seem to work

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


new.vect <- 1:10
sum(new.vect)
