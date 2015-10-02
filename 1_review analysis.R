# code to run analyses and figures of interaction papers database
setwd("/Users/emilydarling/Documents/Work/GitHub/ProcB_Synergies/data3_full dbase")
d <- read.csv("interaction database_ProcB_v3.csv", header = TRUE, stringsAsFactors = FALSE)  

# Darling, Brown 1 Oct 2015

library(dplyr)
library(reshape2)
library(ggplot2)

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


setwd("/Users/emilydarling/Documents/Work/GitHub/ProcB_Synergies/data3_full dbase")

## summary analysis of database for Em's section
#setwd('/Users/s2973410/Code/ProcB_Synergies/data3_full dbase')
d <- read.csv("interaction database_ProcB_v3.csv", header = TRUE, stringsAsFactors = FALSE)  
head(d)
names(d)
nrow(d)

#summary analysis
names(d)[7] <- "year"
min(d$year); max(d$year)

#117 journals
unique(d$journal)

# 616 journal articles
nrow(d)
names(d)

#proportion of interaction statistics
with(d, table(synergy))
424/619

with(d, table(additive))
215/619

with(d, table(antag))
94/61

#2014 summary sentence
with(subset(d, year == 2014), table(synergy))
with(subset(d, year == 2014), table(additive))
with(subset(d, year == 2014), table(antag))

#51 synergies
#21 additive
#13 antag

51 / (51+21+13)

54/34

#select and melt to long
d2 <- d %>%
  select(year, title, journal, type, times_cited, synergy, antag, additive)

head(d2)
names(d2)

d3 <- melt(d2, id.vars = 1:5, variable.name = "interaction")
d3 <- d3[-which(d3$value == 0),]
head(d3)

#733 interactions
nrow(d3)

table(d3$interaction)
424/733
215/733
94/733

unique(d3$type)


d4 <- d3 %>%
  select(year, interaction, value) %>%
  group_by(year, interaction) %>%
  summarize(sum = sum(value))
d4

# plot of interaction types over time
# fig 2 for manuscript
unique(d4$interaction)
d4$interaction <- factor(d4$interaction, levels = c("synergy", "additive","antag"))
int.plot <- ggplot(data = subset(d4, year < 2015), aes(x = year, y = sum)) + 
  geom_line(aes(colour = interaction), size = 1.5) + 
  scale_x_continuous("Year", expand = c(0.01,0.01)) + 
  scale_y_continuous("Number of papers", expand = c(0.05, 0.05)) + 
  theme_bw(base_size = 16) + 
  theme(legend.key = element_rect(colour = NA)) + 
  scale_colour_manual("Interaction type", 
                        labels = c("Synergy", "Additive", "Antagonism"),
                        values = c("red","darkorange","navyblue")) + 
  theme(axis.title.y = element_text(vjust = 1)) + 
  theme(axis.title.x = element_text(vjust = -0.25))

setwd("/Users/emilydarling/Documents/Work/GitHub/ProcB_Synergies/plots")
pdf("interactions over time.pdf", width = 6.5, height = 3.5)
int.plot
dev.off()

# calculate perc. of synergies
head(d4)

d5 <- d4  %>% 
  ungroup() %>%
  group_by(year) %>%
  mutate(n_papers = sum(sum)) %>%
  filter(interaction == "synergy") %>%
  mutate(prop_synerg = sum / n_papers)
d5

# plot of proportion synergy papers over time
# nope, don't do this
ggplot(data = subset(d5, year < 2015), aes(x = year, y = prop_synerg)) + 
  geom_line(colour = "red", size = 1)

# model: are synergies more cited than other interaction types? 
head(d3)
names(d3)

unique(d3$interaction) 
d3$interaction <- factor(d3$interaction, levels = c("additive", "antag","synergy"))

d3$yearsince <- 2015 - d3$year

# Exculding reviews etc...
d5 <- filter(d3, type == 'Article'| type == 'Article; Proceedings Paper' | type == 'Proceedings Paper' | type == 'Letter')

nrow(d5)

# Analysis of citations
#

library(lme4)
library(splines)
library(MASS)


hist(d3$times_cited, 40)
#
# Fixed effects models
#

# Try a glm
m1f <- glm(times_cited ~ interaction + ns(yearsince, 2), data = d5, family = "poisson")
summary(m1f)
# Check for overdispersion = residual deviance / resid df
14909/ 328

#Fit negative bin model
m2f <- glm.nb(times_cited ~ interaction + ns(yearsince, 2), 
data = d5)
summary(m2f)

#Spline not sig, so just use linear for year
m3f <- glm.nb(times_cited ~ interaction + yearsince, 
data = d5)
summary(m3f)

dev.new(width = 8, height = 4)
par(mfrow=c(1,2))
termplot(m3f, se = TRUE, partial.resid=T, col.term = 'black', col.se = 'grey20')

#Test it with AIC
drop1(m2f)

#Get CIs and model means
xc <- confint(m3f)
xmean <- m3f$coefficients

# Plot of effectss

dev.new(width = 8, height = 4)
par(mfrow = c(1,2))



#
# Mixed effects models
#

# Warning: random effect is super unbalanced...
# Fit glmer
m0 <- glmer(times_cited ~ interaction + ns(yearsince, 2)  + (1|journal), data = d3, family = "poisson")
summary(m0)

# Check for overdispersion = residual deviance / resid df
overdisp_fun(m0)

# Try glmmpql
m1 <- glmmPQL(times_cited ~ interaction + ns(yearsince, 2), random = ~ 1|journal, data = d3, family = negative.binomial(theta = 27))

summary(m1)

intervals(m1)


#
# Analysis using pre-melt
#

d2$yearsince <- 2015 - d2$year


m3fa <- glm.nb(times_cited ~ factor(additive) + factor(synergy) + factor(antag) + yearsince, data = d2)
summary(m3fa)


dev.new(width = 5, height = 5)
par(mfrow=c(2,2))
termplot(m3fa, se = TRUE, partial.resid=T, col.term = 'black', col.se = 'grey20')

confint(m3fa)



subset(d3, times_cited > 300)



## old emily analysis for cleaning database
hist(d$check)
d$check <- rowSums(d[,12:14])
subset(d, check == 0)

d <- subset(d, check > 0)
nrow(d)
write.csv(d, "interaction database_ProcB_v3.csv", row.names = FALSE)

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

#117 journals
unique(d$journal)





