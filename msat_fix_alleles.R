file.pathway <- "/Users/" # change this
file.name <- "Compiled_7Nov16.csv"
input <- paste(file.pathway, file.name, sep = "")
# Read in data to data frame
msat.data <- read.csv(input,row.names = 1, header=TRUE)

# Establish the allele slots (1,2,3) and locus names.
allele_slot <- function(x){
  return((unlist(strsplit(x, split = "_"))[[2]]))
}

locus <- function(x){
  return((unlist(strsplit(x, split = "_"))[[1]]))
}

# get list of locus names:
locus.names <- unique(sapply(names(msat.data), locus))

# make changes
for(sample in 1:nrow(msat.data)){
    for(locus in locus.names){ # For each locus
      two <- paste(locus, "_2",sep = "") # rebuild column index for slot 1
      one <- paste(locus, "_1",sep = "") # rebuild column index for slot 2
      #problem with blanks - not fixed (I changed these 4 cells manually to zero)
      if(msat.data[sample,one] != 0 && msat.data[sample,two] == 0){ #Check to see if a zero needs replacing
        msat.data[sample,two] <- msat.data[sample,one]  # if so, copy allele from slot 1 into slot 2
    }
  }
}
write.csv(msat.data, file = paste(file.pathway, "new.msat.csv", sep = ""))
msat.data
