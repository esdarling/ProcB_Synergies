## Simple model of what happens we have have to make choices about managing multiple interactions

# CJ Brown 1 Oct 2015
# EM Darling, I Cote

rm(list = ls())

#Function to model additive outcomes
model <- function(c, a, b, xy){
	#Where:
	# c is interaction effect (comes first so we can use lapply)
	# a is effect of stressor 1
	# b is effect of stressor 2
	# xy is a two column matrix of the levels of stressor 1 and 2
	(a * xy[,1]) + (b * xy[,2]) + (c * xy[,1] * xy[,2])
	}

# Stressor effects

a <- -1
b <- -1
c <- c(-0.5, 0, 0.5) # interaction effects

#Stressor levels
n <- 3
xy <- expand.grid(seq(0,1, length.out = n), seq(0,1, length.out = n))


#
# Model impacts and management scenarios
#

#outcomes all impacts
outfull <-  matrix(unlist(lapply(c, model, a, b, xy)), 
   nrow = 3, byrow = T)

#Outcomes removing stressor 1
xy1 <- xy
xy1[,1] <- 0
outstress1 <- matrix(unlist(lapply(c, model, a, b, xy1)), 
   nrow = 3, byrow = T)


#Outcomes removing stressor 2
xy2 <- xy
xy2[,2] <- 0
outstress2 <- matrix(unlist(lapply(c, model, a, b, xy2)), 
   nrow = 3, byrow = T)

#Outcomes removing equal amounts (half of each in this case) of stressors 1 & 2
xy12 <- xy - (xy/2)

outstress12 <- matrix(unlist(lapply(c, model, a, b, xy12)), 
   nrow = 3, byrow = T)

#
# Calculate benefits
#
ben1 <- outstress1 - outfull 
ben2 <- outstress2 - outfull 
ben12 <- outstress12 - outfull 

#
# Plot comparisons
#
dev.new()
par(mfrow = c(2,2))
barplot(ben1, beside = T)
abline(h = c(0.5,1))
barplot(ben2, beside = T)
abline(h = c(0.5,1))
barplot(ben12, beside = T)
abline(h = c(0.5,1))