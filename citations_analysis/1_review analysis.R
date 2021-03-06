# Code to run analyses of citations for interaction papers database
# Darling, Brown, Cote
# 26 Oct 2015
rm(list = ls())

#Data manip packages
library(dplyr)
library(reshape2)
library(ggplot2)
library(car)

#Analysis packages
library(splines)
library(MASS)

# Function for checking overdispersion in lme4 models
# from: http://glmm.wikidot.com/faq
overdisp_fun <- function(model) {
  ## number of variance parameters in
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}


########################################
# DATA IMPORT
########################################


# setwd("/Users/emilydarling/Documents/Work/GitHub/ProcB_Synergies/data3_full dbase")

setwd('/Users/s2973410/Code/ProcB_Synergies/data3_full dbase')
d <- read.csv("interaction database_ProcB_v3.csv", header = TRUE, stringsAsFactors = FALSE)
head(d)
names(d)
nrow(d)

########################################
# DATA PREP
########################################


names(d)[3] <- "journal"
d$journal <- tolower(d$journal)

#make journal key to check for spelling changes
journal.key <- as.data.frame(x = unique(d$journal))
names(journal.key)[1] <- "journal"
head(journal.key)
journal.key <- journal.key[order(journal.key$journal),]

#write.csv(journal.key, "journal.key.csv")

#recode journal names
d$journal <- recode(d$journal,
"'annual review of ecology, evolution, and systematics, vol 44' = 'annual review of ecology evolution and systematics';
'annual review of ecology, evolution, and systematics, vol 45' = 'annual review of ecology evolution and systematics';
'annual review of ecology, evolution, and systematics, vol 41' = 'annual review of ecology evolution and systematics'")

#Create a new grouped type variable
d$type2 <- d$type
d$type2[d$type == 'Review; Book Chapter'] <- 'Review'
d$type2[d$type == 'Letter'] <- 'Article'
d$type2[d$type == 'Article; Proceedings Paper'] <- 'Article'
d$type2[d$type == 'Proceedings Paper'] <- 'Article'
d$type2[d$type == 'Note'] <- 'Other'
d$type2[d$type == 'Correction'] <- 'Other'
d$type2[d$type == 'Editorial Material'] <- 'Other'

table(d$type2)
#118 journals
unique(d$journal)

#summary analysis
names(d)[7] <- "year"
min(d$year); max(d$year)

#Years since publication
d$yearsince <- 2015 - d$year

# 619 journal articles
nrow(d)

d2 <- d 

head(d2)
names(d2)
unique(d2$type)

# Remove 'other' types of articles
d3 <- d2 %>% filter(type2 != 'Other')

with(d3, table(synergy, antag, additive))

# Create new factor that is composite of interaction types
# Levels are: 'additive only', 'additive and/or synergy', 'additive and/or antagonism', 'synergy and/or antagonism'.

d3$int_type <- rep('Additive only', nrow(d3))
d3$int_type[d3$synergy ==1 & d3$antag ==0] <- 'Synergy only'
d3$int_type[d3$synergy ==0 & d3$antag ==1] <- 'Antagonism only'
d3$int_type[d3$synergy ==1 & d3$antag ==1] <- 'All types'

table(d3$int_type)
table(d3$type2)
########################################
# ANALSYSIS
########################################

#
# Interaction types class analysis
#

#Interaction type * review interaction
m3 <- glm.nb(times_cited ~ factor(int_type)*factor(type2) + ns(yearsince, df=3), data = d3)
summary(m3)

m4 <- update(m3, .~.-factor(int_type):factor(type2))

anova(m3,m4)
drop1(m3)
 # So interaction is signficant and has marginal information, so keep it one...

#
# Plot effects
#

#Data frame for predicting to factors
# Choosing 8 years as baseline, because this is where curve flattens out
newdat <- expand.grid(int_type = unique(d3$int_type)[c(1, 2, 4, 3)], type2 = unique(d3$type2),yearsince = 15)

pred1 <- predict(m3, newdata = newdat, type = 'response', se = T)

#Data frame for years
newdat2 <- expand.grid(int_type = 'Additive only', type2 = 'Article', yearsince = seq(min(d3$yearsince), max(d3$yearsince), length.out = 10))
pred2 <- predict(m3, newdata = newdat2, type = 'response', se.fit = T)

# Make the plot
xvals <- 1:8
labnams <- newdat[,1]
labnams2 <- unique(newdat[,2])

sebars <- cbind(pred1$fit + pred1$se.fit, pred1$fit - pred1$se.fit)


dev.new(width = 8, height = 4)
par(mfrow = c(1,2), las = 1)

# Plot one levels at 15 years
par(mar = c(4, 8, 2, 2))
plot(pred1$fit, xvals, yaxt = 'n', ylab = '', xlab ='Number of citations', pch = 16, cex = 1, xlim = c(0, 220), ylim = c(0.5, 8.5), xaxs = 'i', yaxs = 'i')
#Add shading
lines(c(-112, -112), c(8,4.8), xpd = NA)
lines(c(-112, -112), c(4.3,0.5), xpd = NA)
rect(0.01,0.01,219.9,4.49, col = 'grey80', border = NA)

#Add points back in with error bars
points(pred1$fit, xvals, pch = 16, cex = 1)
arrows(sebars[,1], xvals, sebars[,2], xvals, len=0, lwd = 2)

text(-115, xvals, labnams, xpd = NA, cex = 0.6, pos = 4)
text(-135, c(2.5,6.5), labnams2, xpd = NA, cex = 0.8, srt = 90)



# Plot two, time
par(mar = c(4, 4, 4, 2))
plot(newdat2$yearsince, pred2$fit, type ='l', xlab = 'Years since publication', ylab = 'Number of citations', ylim = c(0, 60))
lines(newdat2$yearsince, pred2$fit + pred2$se.fit, lty = 2)
lines(newdat2$yearsince, pred2$fit - pred2$se.fit, lty = 2)


#
# Calculate some stats
#
pred1$fit[1]/mean(pred1$fit[c(2:4)])
pred1$fit[1]/mean(pred1$fit[c(5:8)])
