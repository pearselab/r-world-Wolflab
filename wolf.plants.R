setup.plants <- function(repro, survive, comp.mat, names=NULL){
  if(is.null(names))
    names <- letters[seq_along(repro)]
  if(length(repro) != length(survive))
    stop("Reproduction and survival parameters needed for all species")
  #...some more tests...
  repro <- setNames(repro, names)
  #...what does the line above do? Do you want more like it?
  return(list(repro=repro, survive=survive, comp.mat=comp.mat,
              names=names))
}

repro <- c(0.1, 0.4, 0.6)
survive <- c(0.5, 0.7, 0.7)
comp.mat <- matrix(0.5, nrow = 3, ncol = 3)
names <- list("Ipomopsis", "Cheilanthes", "Papaver")

print(setup.plants(repro, survive, comp.mat, names))
