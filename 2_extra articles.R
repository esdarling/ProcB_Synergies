# Oct 1 database
setwd("/Users/emilydarling/Documents/Work/GitHub/ProcB_Synergies/data3_full dbase")
d <- read.csv("interaction database_ProcB_v2.csv", header = TRUE, stringsAsFactors = FALSE)  
head(d)
names(d)
nrow(d)

# 1042 driver* and disturbance* papers to compare and add in
f <- read.csv("1042_to check.csv", header = TRUE, stringsAsFactors = FALSE)  
head(f)
names(f)
nrow(f)

f$title <- tolower(f$title)
f$abstract <- tolower(f$abstract)

#anti-join finds articles that did not merge into original database
names(f)
merge <- anti_join(f,d, by = "title")
head(merge)
write.csv(merge, "test.csv", row.names = FALSE)

#set up single greps for each interaction
d <- merge
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
d$check <- rowSums(d[12:14])
head(d$check)

d2 <- subset(d, check > 0)
nrow(d2)

#write database and manually confirm interactions, remove non-interaction papers
write.csv(d2, "328 extra articles to check_Oct1.csv", row.names = FALSE)


#manually copy additive into word, set text to red colour
#copy back into excel and search manually

#reload manually checked interactions back into R
setwd("/Users/emilydarling/Documents/Work/GitHub/ProcB_Synergies/data2_grep_interactions_checked")
extra <- read.csv("328 extra articles_grep.csv", header = TRUE, stringsAsFactors = FALSE)  
head(extra)
names(extra)
nrow(extra)

extra$check <- rowSums(extra[,12:14])
head(extra$check)
hist(extra$check)
extra <- subset(extra, check > 0)

names(extra)

str(d)
str(extra)

extra$BP <- as.character(extra$BP)
#add extras into interaction database
final <- bind_rows(d, extra)

setwd("/Users/emilydarling/Documents/Work/GitHub/ProcB_Synergies/data3_full dbase")
write.csv(final, "interaction database_ProcB_v3.csv", row.names = FALSE)






