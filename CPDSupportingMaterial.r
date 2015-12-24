library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
options(scipen=3)

# load the necessary files into this R session
#---------------------------------------------

setwd('C:/Users/elundquist/Documents/R')
load('./RData/CPDAnalysisFile.rdata')
load('./RData/CPDCoordinatesFile.rdata')
load('./RData/CPDSummaryFile.rdata')

# produce whatever supporting diagnostic material is necessary for the report
#----------------------------------------------------------------------------

summary(parkdays$num_crime)
round(100*table(parkdays$num_events),2)/nrow(parkdays)

group_by(parkdays, park_class) %>% summarize_each(funs(mean), num_crime:num_crime_violent) %>% arrange(num_crime)
# NOTE: only CITYWIDE/MAGNET parks have daily crime rates higher than one

ggplot(data = parkdays) + geom_histogram(aes(x = num_crime), binwidth = 1) + coord_cartesian(xlim = c(0, 10)) + 
  labs(x = 'Crimes/Day', y = 'Frequency', title = 'Distribution of Crimes/Day - All Parks')
# NOTE: we have a massive zero-censoring problem in our data

ccdensity <- matrix(ncol = 2, nrow = 0)
for (i in 0:10) {
  ccdensity <- rbind(ccdensity, c(i, length(parkdays$num_crime[parkdays$num_crime == i])/length(parkdays$num_crime)))
}

ccdensity          
colSums(ccdensity)
# NOTE: density of crimes is basically [D = 2^-(C+1)] - halves as you increase crime counts per day

group_by(parkdays, park_class) %>% summarize_each(funs(mean), num_events) %>% arrange(num_events)
# NOTE: MAGNET parks have roughly 2 events per day - others at roughly an event every 10-20 days

# produce a histogram on crimes with an overlaid poisson density plot

c.hist <- ggplot(data = parkdays) + geom_histogram(aes(x = num_crime), ) + coord_cartesian(xlim = c(0, 20))

c.lambda <- mean(parkdays$num_crime)
t.probs  <- dpois(0:10, c.lambda)
