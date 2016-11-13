# Goal of this code:
# diploid organisms have two alleles
# genotyping software will assign one allele to a homozygote
# This leaves the second "allele slot" empty, which is misleading for downstream analyses.
# Here we simply look to see if the first slot has an allele (not zero)
# Then check second slot. If second slot is zero (meaning that homozygote), then change second slot to same allele as first
# Note that this species ocassionally has triploid plants, hence the thrid allele slot
# Just ignore third slot for time being

file.pathway <- "/Users/Paul13/Dropbox/docs_wolf/Python_files/2016_Programming/r-world-Wolflab/" # change this
file.name <- "Compiled_7Nov16.csv"
input <- paste(file.pathway, file.name, sep = "")
# Read in data to data frame
msat.data <- read.csv(input,row.names = 1, header=TRUE)

# convert all NA to zero:
msat.data[is.na(msat.data)]<- 0

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
# This version (below) works. Carol's solution. Will, I don't think this is looping through the data frame?

for(locus in locus.names){ # For each locus
  two <- paste(locus, "_2",sep = "") # rebuild column index for slot 1
  one <- paste(locus, "_1",sep = "") # rebuild column index for slot 2
  print(one)
  print(two)
  msat.data[,two] <- ifelse((msat.data[,one] > 0 & msat.data[,two] == 0), msat.data[,one], msat.data[,two])
}

# This ugly loop below works perfectly:
# for(sample in 1:nrow(msat.data)){
#     for(locus in locus.names){ # For each locus
#       two <- paste(locus, "_2",sep = "") # rebuild column index for slot 1
#       one <- paste(locus, "_1",sep = "") # rebuild column index for slot 2
#       if(msat.data[sample,one] != 0 && msat.data[sample,two] == 0){ #Check to see if a zero needs replacing
#         msat.data[sample,two] <- msat.data[sample,one]  # if so, copy allele from slot 1 into slot 2
#     }
#   }
# }
write.csv(msat.data, file = paste(file.pathway, "new.msat2.csv", sep = ""))
# msat.data
#that worked



