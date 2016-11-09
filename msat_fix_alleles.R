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
# Tried multple attempts with "with" and lapply. Can get it to throw no errors but nothing gets changed
# Cannot locate my errors, So lets try a loop:

for(sample in 1:nrow(msat.data)){
    for(locus in locus.names){ # For each locus
      two <- paste(locus, "_2",sep = "") # rebuild column index for slot 1
      one <- paste(locus, "_1",sep = "") # rebuild column index for slot 2
      if(msat.data[sample,one] != 0 && msat.data[sample,two] == 0){ #Check to see if a zero needs replacing
        msat.data[sample,two] <- msat.data[sample,one]  # if so, copy allele from slot 1 into slot 2
    }
  }
}
write.csv(msat.data, file = paste(file.pathway, "new.msat.csv", sep = ""))
msat.data
#that worked

# Try to do this with "with"
# correct.homozygous.allele <- function(locus.names){
#       for(locus in locus.names){ # For each locus
#         two <- paste(locus, "_2",sep = "") # rebuild column index for slot 1
#         one <- paste(locus, "_1",sep = "") # rebuild column index for slot 2
#         if(msat.data[sample,one] != 0 && msat.data[sample,two] == 0){ #Check to see if a zero needs replacing
#           msat.data[sample,two] <- msat.data[sample,one]  # if so, copy allele from slot 1 into slot 2
#       }
#     }
# }
# 
# with(msat.data, correct.homozygous.allele(locus.names))

# error with above is "Error in xj[i] : invalid subscript type 'closure'"
# still cannot get full traceback call. Must determine how to do that
# This still better than earlier attempts with lapply - no error but function failed to change df