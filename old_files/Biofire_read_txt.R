#read in BIOFIRE RESULTS txt data & parse
#stole code from here https://support.bioconductor.org/p/66724/
#file.choose to pick txt file
#need to merge with enrollment sheet & autofill with Pt_ID (MRN??) & group (1-6)
library(stringr)
library(qdap)
library (tidyr)
#reset vars
posgi = ''
posgi1 = ''
posgi2 = ''

lns = readLines (file.choose(new =FALSE), warn=FALSE)
print (lns)
#df <- readLines(text = lns, skip = 8L, strip.white = TRUE, nrows = 30)
df <- read.csv(text=lns,  stringsAsFactors=FALSE, skip = 8L, strip.white = TRUE, nrows = 30)
id <- head (df,1)
print(id)

#line 3 has the overall results
#set flag for no GI pathogens
posgi <- substr (df[3, 1], 1, 17)
print(posgi)
#set conditional Detected = 1, None = 0 otherwise blank
  posgi1 = ifelse (posgi != "Detected:    None", 1, "")
  posgi2 = ifelse (posgi == "Detected:    None", 0, "")
print (posgi1)
print (posgi2)  
  if (posgi1== 1) {
    df$posgi <- posgi1
  } else if (posgi2== 0) {
    df$posgi <- posgi2 
  } else
    df$posgi <- ""

# extract Sample ID
sampid <- substr (id, 15, 17)
print (sampid)
df$sampid <- sampid

# Get RUNDATE, put in new col
rundate <- sub('.*:', '', id)
print (rundate) # ARRRGH has extra quotes!!!!
df$rundate <- rundate



