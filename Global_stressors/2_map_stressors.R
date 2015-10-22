# Make the map of the number of stressor interactions
# Using number of pressures raster created in 'read_stressors.R'
# CJ Brown, E Darling, I Cote. 30 Sept 2015


rm(list = ls())

library(raster)
library(RColorBrewer)
library(maps)

#
# Function to make (more) continuous palettes from categories of colours
#

pal.cont <- function(pal, n){
	pal <- colorRampPalette(pal)
	pal(n)
	}

#
# Load data
#

setwd('/Users/s2973410/Databases/Global stressors/')
rsum.load<- raster('num_stressors_quant10.grd')

#
#Resample to speed things up for test runs
#
rsum <- sampleRegular(rsum.load, size = 1000000, asRaster = T)
rsum[rsum[]==0] <- NA #get rid of NAs

#Max number of stressors
maxn <- rsum@data@max

# Choose can determine how many combinates. 
# First param is number of classes to choose from
# Second param is number of choices. E.g. following plots number of combinations of maxn numbers taking from 1 to maxn  numbers
# plot(1:maxn, cumsum(choose(maxn, 1:maxn)))

#Number of possible interactions starting at one way (ie none), then 2 way interactions etc...
nints_2way <- c(0, choose(1:maxn, 2))
nints_3way <- c(0, choose(1:maxn, 3))
nints_2_3way <- nints_2way #+ nints_3way

rcl <- cbind(0:maxn, nints_2_3way)
#This step takes about a 90 seconds if using full dataset
system.time(rints <- reclassify(rsum, rcl))

maxint <- max(nints_2_3way)
#
# Figure params
#
colNA <- 'white'

#Number stressors plot
colsum <- pal.cont(brewer.pal(8, 'BuPu'), maxn)
numbreaks <- c(0,1:maxn)

#Number interactions plot
# colint <- rev(pal.cont(brewer.pal(8, 'RdBu')[c(1:4, 7)], 11))
colint <- brewer.pal(8, 'RdBu')[c(7,6,4,3,2,1)]
numbreaks.int <- c(0, 1, 10, 50, 100, 200)#round(c(0, 10, seq(1, 200, length.out = 10)))
# pie(rep(1, 8), col = brewer.pal(8, 'RdBu'))

#lines plot
lwduse <- 2
collines <- c('black','orange', 'red')

#
# Set up and plot the figure
#
dev.new(width = 11, height = 3)
par(mfrow = c(1,2), mar = c(2,2,1,6))

# Will need to use layout and 'legend.only = T' to position legends

#Part 1, number of stressors
plot(rsum, colNA = colNA, col = colsum, breaks = numbreaks, lab.breaks = NA, xaxt = 'n', yaxt = 'n', axis.args = list(at = c(1, 5, 10, 15), labels = T), legend.width = 1.5, asp = NA, bty = 'n')

#Part 2, number of interactions
plot(rints, colNA = colNA, col = colint, breaks = numbreaks.int, lab.breaks = NA, xaxt = 'n', yaxt = 'n', axis.args = list(at = c(1, 50, 100, 150, 200), labels = T), legend.width = 1.5, asp = NA, bty = 'n')

#Part 3, subset, relationship num stressors to num interactions
plotdim <- par("plt")
xleft <- plotdim[2]*0.69
xright <- plotdim[2]*0.8
ybottom <- plotdim[3]
ytop <- plotdim[4]*0.4

# set position for inset
par(
  fig = c(xleft, xright, ybottom, ytop)
  , mar=c(0,0,0,0)
  , new=TRUE
  )

# add inset
par(las = 1)
plot(rcl, xlab = 'Number of stressors', ylab = 'Number of interactions', type = 'l', lwd = lwduse, col = collines[1], bg = 'white', xaxp = c(0, maxn, 1), yaxp = c(0, maxint, 1), cex.axis = 0.7)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
"white")
 lines(rcl, lwd = lwduse, col = collines[1])
# lines(rcl[,1], nints_2way, lwd = lwduse, col = collines[2])
# lines(rcl[,1], nints_3way, lwd = lwduse, col = collines[3])

# legend('topleft', legend = c('2 and 3-way','2-way', '3-way'), lwd = 2, col = collines, cex = 0.5, bty = 'n')

text(8, -46, paste('Number of','\n',' stressors'), xpd = NA, cex = 0.6)

text(-6, 77, paste('Number','\n',' of 2-way','\n',' interactions'), xpd = NA, cex = 0.6, srt = 90)

