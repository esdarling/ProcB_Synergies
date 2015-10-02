
# code to run analyses and figures of interaction papers database
setwd("/Users/emilydarling/Documents/Work/GitHub/ProcB_Synergies/data3_full dbase")
d <- read.csv("interaction database_ProcB_v3.csv", header = TRUE, stringsAsFactors = FALSE)  
head(d)
names(d)
nrow(d)

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

#summary analysis
names(d)[7] <- "year"
min(d$year); max(d$year)

# 619 journal articles
nrow(d)

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

library(splines)

d3$yearsince <- 2015 - d3$year

m0 <- glm(times_cited ~ interaction + ns(yearsince, 1), data = d3, family = "quasipoisson")
summary(m0)

par(mfrow = c(1,2))
termplot(m0, se = T)


with(d3, boxplot(times_cited ~ yearsince))

with(d3, boxplot(times_cited/yearsince ~ interaction))

subset(d3, times_cited > 300)




