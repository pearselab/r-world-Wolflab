setup.plants <- function(repro, survive, comp.mat, names=NULL){
  # Note that I really like this dfensive programming stuff!
  if(is.null(names))
    names <- letters[seq_along(repro)]
  if(length(repro) != length(survive))
    stop("Reproduction and survival parameters needed for all species")
  if(ncol(comp.mat) != length(survive))
    stop("you dimwit, check your matrix size versus survive vector")
  if(any(survive > 1.0) | (any(survive < 0)))
    stop("Silly noodle: survive probabilities cannot be greater than 1.0 or less than zero")
  if(any(repro > 1.0) | (any(repro < 0)))
    stop("Silly noodle: repro probabilities cannot be greater than 1.0 or less than zero")
  #...some more tests...
  repro <- setNames(repro, names)
  survive <- setNames(survive, names)
  #...what does the line above do? Do you want more like it?
  #... Nice but should not work for comp.mat - too many elements?
  #comp.mat <-setNames(comp.mat, names)
  # no idea what that is doing. Just gave NA. Don't work
  return(list(repro=repro, survive=survive, comp.mat=comp.mat,
              names=names))
}

repro <- c(0.1, 0.4, 0.6)
sum(repro)
survive <- c(0.5, 0.7, 0.7)
comp.mat <- matrix(0.5, nrow = 3, ncol = 3)
names <- list("Ipomopsis", "Cheilanthes", "Papaver")

info <- setup.plants(repro, survive, comp.mat, names)
print(info)


test <- c(0.1, 0.4, 0.6, -.3)
if(any(test > 1.0) | (any(test < 0))){
  print("messed up")
}else{
  print("ok mate")
}

runif(6,5,17)
