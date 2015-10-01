# code to grepl for synerg*, addit* or antag*
toMatch <- c("synerg", "addit", "antag")
title <- as.character("This is a test of a synergistic and antagonistic title")

grep(paste(toMatch, collapse = "|"), title)
grep("synerg", title)
grep("antag", title)

#grep finds matches
#grep is seaching titles and creating a vector where the substring occurs
#e.g., 47 titles have synergy, antag, addit in them, and here is that subset
test <- grep(paste(toMatch, collapse = "|"), d$title, ignore.case = TRUE, value = FALSE)
head(test)
length(test)

?grep

# Read in Web of Science database
## this analysis was done in two parts, before 2010 and after 2010
setwd("/Users/emilydarling/Documents/Work/GitHub/ProcB_Synergies/data1_from_WOS")
d <- read.csv("WOS abstracts_up to 2010.csv", header = TRUE, stringsAsFactors = FALSE)  
#d <- read.csv("WOS abstracts_2011 to 2015.csv", header = TRUE, stringsAsFactors = FALSE)  
head(d)
names(d)
nrow(d)

#set title and abstract to lower case for string searches
d$title <- tolower(d$title)
d$abstract <- tolower(d$abstract)

#set up single greps for each interaction
head(d)
isynergy <- c(grep("synerg", d$title), grep("synerg", d$abstract))
d$synergy <- 0
d$synergy[isynergy] <- 1

iantag <- c(grep("antagonis", d$title), grep("antagonis", d$abstract))
d$antag <- 0
d$antag[iantag] <- 1

#additiv, remove "additional"
iadd <- c(grep("additiv", d$title), grep("additiv", d$abstract))
d$additive <- 0
d$additive[iadd] <- 1

# remove papers with no synerg, additive, antag
names(d)
d$check <- rowSums(d[14:16])
head(d$check)

d2 <- subset(d, check > 0)
nrow(d2)

#write database and manually confirm interactions, remove non-interaction papers
write.csv(d2, "grep test_up to 2010.csv", row.names = FALSE)
write.csv(d2, "grep test_up to 2010.csv", row.names = FALSE)

#manually copy additive into word, set text to red colour
#copy back into excel and search manually

#reload manually checked interactions back into R
setwd("/Users/emilydarling/Documents/Work/GitHub/ProcB_Synergies/data2_grep_interactions_checked")
d2010 <- read.csv("up to 2010_grep.csv", header = TRUE, stringsAsFactors = FALSE)  
head(d2010)
names(d2010)
nrow(d2010)

d2011 <- read.csv("2011_2015_grep.csv", header = TRUE, stringsAsFactors = FALSE)  
head(d2011)
names(d2011)
nrow(d2011)

#bind pre-2010 and 2011-2015 databases together
dfull <- bind_rows(d2010,d2011)

dfull$check <- rowSums(dfull[,14:16])
head(dfull$check)
hist(dfull$check)
dfull <- subset(dfull, check > 0)

setwd("/Users/emilydarling/Documents/Work/GitHub/ProcB_Synergies/data3_full dbase")
write.csv(dfull, "interaction database_ProcB_v2.csv", row.names = FALSE)



