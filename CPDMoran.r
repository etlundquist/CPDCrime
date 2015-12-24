# set enviornment variables and load required packages
#-----------------------------------------------------

setwd('C:/Users/elundquist/Documents')
source('./R/Authentication/gpcnx.r')
library(RPostgreSQL)
library(readr)
library(dplyr)
library(geosphere)
library(ape)
library(ggplot2)

# begin a connection to the project's GP database
#------------------------------------------------

cnx <- dbConnect(dbDriver("PostgreSQL"),
       host     = gp.host, 
       port     = gp.port, 
       dbname   = 'scratch', 
       user     = gp.user, 
       password = gp.password)

# pull the park coordinates from our GP Schema
#---------------------------------------------

dbSendQuery(cnx, 'SET search_path TO cpd')
parks <- postgresqlReadTable(cnx, 'parks', row.names = NULL)
names(parks) <- make.names(names(parks))
parks <- select(parks, park_number, latitude, longitude)

# bring in the park/day level events & crime counts data
#-------------------------------------------------------

parkdays <- read_csv('T:/CPD_Team/Data Files/noEventsAndCrimesPerParkPerDay.csv', col_names = TRUE)
dim(parkdays[is.na(parkdays$park),]) # NOTE: some missing park numbers in this file

cors <- lapply(split(parkdays[,c('eventflag','numberofcrimes')], parkdays$park), function(x) cor(x)[2,1])
parkcors <- data.frame(park_number = as.numeric(names(cors)), correlation = as.numeric(cors))
stopifnot(dim(parkcors)[1] == length(unique(parkdays$park[!is.na(parkdays$park)])))
# NOTE: this code is correlating events/crime separately for every park and creating a DF of results

summary(parkcors$correlation)
parkcors <- filter(parkcors, !is.na(correlation))

# merge together the correlation and coordinates data
#----------------------------------------------------

parkstats <- inner_join(parks, parkcors, by = 'park_number')
rm(parks, parkdays, parkcors, cors) # remove no longer needed objects
# NOTE: for now limit to only parks found in both data sets

filter(parkstats, is.na(latitude) | is.na(longitude))
parkstats <- filter(parkstats, !is.na(latitude) & !is.na(longitude))
dim(parkstats) # NOTE: parks with missing coordinates are removed here

group_by(parkstats, latitude, longitude) %>% filter(n() > 1) %>% arrange(latitude, longitude)
parkstats <- group_by(parkstats, latitude, longitude) %>% filter(row_number() == 1)
# NOTE: only keep one park in duplicate coordinate sets

# create a matrix to store the distances between all pairs of parks
#------------------------------------------------------------------

radius_miles <- 3959 # this is the radius of the earth in miles
distances <- matrix(data = NA, nrow = dim(parkstats)[1], ncol = dim(parkstats)[1])
allparks <- as.matrix(parkstats[,c('longitude','latitude')])
# initialize a matrix to hold all (i,j) distances and a matrix with the coordinates of all parks

for (i in 1:dim(parkstats)[1]) {
  
  curpark <- as.numeric(parkstats[i, c('longitude','latitude')])
  distances[,i] <- distHaversine(curpark, allparks, r = radius_miles)
  # calculate the distance between this park and all parks in the data
  # and add it as column (i) in the distance matrix
  
}

stopifnot(isSymmetric(distances)) # QA: this matrix should be symmetrical (distance is reflexive)
stopifnot(diag(distances) == 0)   # QA: diagonals should be zero (distance from park to itself)

head(parkstats, 5)      # QA: do some external checking to make sure these distances are correct
distances[1:5, 1:5] # QA: and the matrix is constructed according to the desired indexes

summary(as.numeric(distances)) # QA: summary statistics on the (i,j) distances
hist(as.numeric(distances))    # QA: histogram of the (i,j) distances
  
# create a series of spatial weight matrices (W) based on the distance matrix calculated above
#---------------------------------------------------------------------------------------------

# W1: weight by inverse distance function
W1 <- 1/distances
diag(W1) <- NA
summary(as.numeric(W1))
diag(W1) <- 0

# W2: weight by exponential decay function
W2 <- exp(-distances)
diag(W2) <- NA
summary(as.numeric(W2))
diag(W2) <- 0

# look at some sample distances and how they would get weighted under both functional forms
distances[1:4,1:4]
W1[1:4,1:4]
W2[1:4,1:4]

# create a function to calculate global Moran's I
#------------------------------------------------

GlobalMoran <- function(measure, W) {
  
  dim       <- length(measure)
  mean      <- mean(measure)
  numerator <- matrix(data = NA, nrow = dim, ncol = dim)
  
  for (i in 1:dim) {
    for (j in 1:dim) {
      numerator[i,j] <- W[i,j] * (measure[i] - mean) * (measure[j] - mean)
    }
  }
  
  sumsquares  <- sum((measure - mean)^2)
  sumweights  <- sum(as.numeric(W))
  GlobalMoran <- (dim/sumweights) * (sum(as.numeric(numerator))/sumsquares)
  GlobalMoran
  
}

# call the function with our data and compare it to the APE package
#------------------------------------------------------------------

GlobalMoran(parkstats$correlation, W2)
Moran.I(parkstats$correlation, W2)

# create a function to calculate Local Moran's I
#-----------------------------------------------

LocalMoran <- function(measure, W) {
  
  dim        <- length(measure)
  mean       <- mean(measure)
  LocalMoran <- numeric()
  
  for (i in 1:dim) {
    t1 <- (measure[i]-mean) / (sum((measure-mean)^2)/dim)
    t2 <- (sum(W[i,] * (measure-mean)))
    LocalMoran  <- c(LocalMoran, t1 * t2) 
  }
  LocalMoran
}

LocalM <- LocalMoran(parkstats$correlation, W2)
summary(LocalM)

# Create a Moran Plot of the  Event/Crime Correlation Data
#---------------------------------------------------------

neighborhood <- numeric(length = length(parkstats$correlation))
for (i in 1:length(parkstats$correlation)) {
  neighborhood[i] <- weighted.mean(parkstats$correlation, W2[i,])
}

parkstats$neighborhood <- neighborhood
fm <- lm(neighborhood ~ correlation, data = parkstats)
fm$coefficients # NOTE: this regression coefficient should equal Moran's I
cor.test(parkstats$correlation, parkstats$neighborhood)

ggplot(data = parkstats) + aes(x = correlation, y = neighborhood) + geom_smooth(method='lm') + geom_point() + 
labs(title = 'Moran Plot of Crime/Event Correlation', x = 'Park Value', y = 'Neighborhod Value')


