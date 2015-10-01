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

## Read in Web of Science database
# 507 papers, 2011 - 2015
d <- read.csv("WOS abstracts_2011 to 2015.csv", header = TRUE, stringsAsFactors = FALSE)  
head(d)
names(d)
nrow(d)

#set all to lower case!! 
d$title <- tolower(d$title)
d$abstract <- tolower(d$abstract)


#set up single greps
head(d)
isynergy <- c(grep("synerg", d$title), grep("synerg", d$abstract))
d$synergy <- 0
d$synergy[isynergy] <- 1

iantag <- c(grep("antag", d$title), grep("antag", d$abstract))
d$antag <- 0
d$antag[iantag] <- 1

#additiv, remove "additional"
iadd <- c(grep("additiv", d$title), grep("additiv", d$abstract))
d$additive <- 0
d$additive[iadd] <- 1
#then manually filter out 'non-additive' later

# remove papers with no synerg, additive, antag
names(d)
d$check <- rowSums(d[14:16])
head(d$check)

d2 <- subset(d, check > 0)
nrow(d2)

write.csv(d2, "grep test.csv", row.names = FALSE)

#manually copy additive into word, set text to red colour
#copy back into excel and search manually


#reload manually checked interactions back into R
d3 <- read.csv("2011_2015_grep.csv", header = TRUE, stringsAsFactors = FALSE)  
head(d3)
names(d3)
nrow(d3)

d3$check <- rowSums(d3[14:16])
head(d3$check)

d3 <- subset(d3, check > 0)
nrow(d3)

#290 papers with confirmed interactions between 2011 and 2015

colSums(d3[14:16])

names(d3)
nrow(d3)

# rearrange to copy into pre-2011 database
names(d3)

unique(d3$type)
d3$review <- ifelse(d3$type == "Review" | d3$type == "Review; Book Chapter" | 
                      d3$type == "Editorial Material",1,0)

d3$pages <- paste(d3$BP, d3$EP, sep = "-")
head(d3$pages)

names(d3)
##add time cited!!
d3.export <- d3[,c(1,8,2,3,9,19,15,16,14,18,7)]
names(d3.export)
write.csv(d3.export, "export 2011 to 2015.csv", row.names = FALSE)


## quick summary statistics of 2011 - 2015
d4 <- melt(d3[-17], id.vars = 1:13, variable.name = "interaction")
d4 <- subset(d4, value > 0)
head(d4)

d5 <- d4 %>%
  select(PY, interaction, value) %>%
  group_by(PY, interaction) %>%
  summarize(sum = sum(value))
d5

ggplot(data = d5, aes(x = PY, y = sum)) + 
  geom_line(aes(colour = interaction))





