
# code to run analyses and figures of interaction papers database
setwd("/Users/emilydarling/Documents/Work/GitHub/ProcB_Synergies/data3_full dbase")
d <- read.csv("interaction database_ProcB_v2.csv", header = TRUE, stringsAsFactors = FALSE)  
head(d)
names(d)
nrow(d)

names(d)[3] <- "journal"
d$journal <- tolower(d$journal)
unique(d$journal)

journal.key <- as.data.frame(x = unique(d$journal))
journal.key <- order(journal.key)

write.csv(journal.key, "journal.key.csv")


#recode journal names
d$journal <- recode(d$journal, 
"'acta oecologica-international journal of ecology' = 'acta oecologica';
'agriculture, ecosystems and environment' = 'agriculture ecosystems and environment';
'annual review of ecology, evolution, and systematics' = 'annual review of ecology evolution and systematics';
'annual review of ecology, evolution, and systematics, vol 44' = 'annual review of ecology evolution and systematics';
'annual review of ecology, evolution, and systematics, vol 45' = 'annual review of ecology evolution and systematics';
'annual review of ecology, evolution, and systematics, vol 41' = 'annual review of ecology evolution and systematics';
'proceedings of the national academy of sciences' = 'pnas';
'proceedings of the national academy of sciences of the united states of america' = 'pnas';
'proceedings of the royal society b-biological sciences' = 'procb';
'proceedings of the royal society of london series b-biological sciences'='procb';
'rangeland ecology & management' = 'rangeland ecology and management';
'trends in ecology & evolution' = 'trends in ecology and evolution'")

journal.key <- as.data.frame(x = unique(d$journal))
write.csv(journal.key, "journal.key.csv")

#186 journals
unique(d$journal)

#procB time analysis
names(d)
d2 <- melt(d[,-12], id.vars = c(1:7,11), variable.name = "interaction")
names(d2)
head(d2)

d2 <- subset(d2, value > 0)
head(d2)

min(d2$year); max(d2$year)
nrow(d2)
unique(d2$journal)

head(d2)

d3 <- d2 %>%
  select(year, interaction, value) %>%
  group_by(year, interaction) %>%
  summarize(sum = sum(value))
d3

ggplot(data = d3, aes(x = year, y = sum)) + 
  geom_line(aes(colour = interaction), size = 1)


#fuck, methods different? 
pre2011 <- subset()







