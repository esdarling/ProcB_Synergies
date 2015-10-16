# code to summarize methods and how interactions are tested
setwd("/Users/emilydarling/Documents/Work/GitHub/ProcB_Synergies/data4_methods")
d <- read.csv("methods_from 2010 database.csv", header = TRUE, stringsAsFactors = FALSE)  
head(d)
names(d)
nrow(d)

## TO DO:: Add number of papers into methods 
## Check if with(table) code is actually working.. 

#check multiple methods in a single paper
#names(d)
#d$check <- rowSums(d[,5:8])
#hist(d$check)
#subset(d, check > 1)

#total, n = 460 papers
d2 <- melt(d[-c(2:4)], id.vars = 1, variable.name = "method")
d2 <- d2[-which(d2$value == 0),]
d3 <- as.data.frame(with(d2, table(method)/460*100), with(d2, table(method)))

d3 <- as.data.frame(with(d2, table(method)))

d3$type <- "total"
d3$no_papers <- as.numeric(row.names(d3))
d3

203+185+108+31

#393 synergy papers
synergy <- subset(d, synergy == 1)
nrow(synergy)
length(unique(synergy$id))

synergy <- synergy[,-c(2:3)]
head(synergy)

synergy2 <- melt(synergy, id.vars = 1:2, variable.name = "method")
synergy2 <- synergy2[-which(synergy2$value == 0),]
synergy3 <- as.data.frame(with(synergy2, table(method)/393*100), 
                          with(synergy2, table(method)))
synergy3$type <- "synergy"
synergy3$no_papers <- as.numeric(row.names(synergy3))
synergy3

#133 additive papers
head(d)
add <- subset(d, additive == 1)
nrow(add)
length(unique(add$id))

head(add)
add <- add[,-c(2,4)]

add2 <- melt(add, id.vars = 1:2, variable.name = "method")
add2 <- add2[-which(add2$value == 0),]
add3 <- as.data.frame(with(add2, table(method)/133*100),
                      with(add2, table(method)))
add3$type <- "additive"
add3$no_papers <- as.numeric(row.names(add3))
add3

#92 additive papers
head(d)
antag <- subset(d, antagonism == 1)
nrow(antag)
length(unique(antag$id))

head(antag)
antag <- antag[,-c(3,4)]

antag2 <- melt(antag, id.vars = 1:2, variable.name = "method")
antag2 <- antag2[-which(antag2$value == 0),]
antag3 <- as.data.frame(with(antag2, table(method)/92*100),
                        with(antag2, table(method)))
antag3$type <- "antagonism"
antag3$no_papers <- as.numeric(row.names(antag3))
antag3

#combine into summary
dat <- bind_rows(d3,synergy3,add3,antag3)
dat <- dat[,c(3,1,4,2)]
head(dat)
write.csv(dat, "methods_summary.csv", row.names = FALSE)

#relevel for ggplot
levels(dat$method)
dat$method <- factor(dat$method, 
                     levels = c("not_tested","glm_interactions","null.model","other"))

levels(as.factor(dat$type))
dat$type <- factor(dat$type, levels = c("total","synergy","additive","antagonism"))

#ggplot
methods.plot <- ggplot(data = dat, aes(x = method, y = Freq)) + 
  geom_bar(aes(fill = type), stat = "identity", position = "dodge") +
  theme_bw(base_size = 20) +
  scale_fill_manual("Interaction type", 
                      labels = c("All interactions","Synergy", "Additive", "Antagonism"),
                      values = c("grey","red","darkorange","navyblue")) +
  scale_x_discrete("Method", expand = c(0.01,0.01), 
                   labels = c("Not tested","Statistical interactions","Null model","Other")) + 
  scale_y_continuous("% of papers", limits = c(0,60), expand = c(0,0)) + 
  theme(axis.title.y = element_text(vjust = 1)) + 
  theme(axis.title.x = element_text(vjust = -0.25))
methods.plot



setwd("/Users/emilydarling/Documents/Work/GitHub/ProcB_Synergies/plots")
pdf("Fig 2b_methods_bar plot.pdf", width = 10, height = 6)
methods.plot
dev.off()








