library(dplyr)
library(readr)
library(stringr)
options(scipen=3)

# load the necessary files into this R session
#---------------------------------------------

setwd('C:/Users/elundquist/Documents/R')
load('./RData/CPDAnalysisFile.rdata')
load('./RData/CPDCoordinatesFile.rdata')
load('./RData/CPDSummaryFile.rdata')

# run a series of regression models relating events and crime
#------------------------------------------------------------

# 1. baseline model with any event as the only predictor variable

ols1sum <- summary( lm(num_crime ~ anyevent, data = parkdays))
psn1sum <- summary(glm(num_crime ~ anyevent, data = parkdays, family = 'poisson'))

round(ols1sum$coefficients[2,], 3)
round(psn1sum$coefficients[2,], 3)
round(exp(psn1sum$coefficients[2,1]), 3) 

# 2. add month indicators to control for seasonality in the crime/event data

ols2sum <- summary( lm(num_crime ~ anyevent + month, data = parkdays))
psn2sum <- summary(glm(num_crime ~ anyevent + month, data = parkdays, family = 'poisson'))

round(ols2sum$coefficients[2,], 3)
round(psn2sum$coefficients[2,], 3)
round(exp(psn2sum$coefficients[2,1]), 3) 

# 3. add park fixed effects to control for park-specific factors (measure crime changes with-parks over time)

ols3sum <- summary( lm(num_crime ~ anyevent + month + parkfactor, data = parkdays))
psn3sum <- summary(glm(num_crime ~ anyevent + month + parkfactor, data = parkdays, family = 'poisson'))

round(ols3sum$coefficients[2,], 3)
round(psn3sum$coefficients[2,], 3)
round(exp(psn3sum$coefficients[2,1]), 3) 

# 4. limit the sample to only parks with at least [5,10,25] events as a sensitivity check

ols4.1sum <- summary( lm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, sum_eventdays >= 5)))
psn4.1sum <- summary(glm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, sum_eventdays >= 5), family = 'poisson'))

ols4.2sum <- summary( lm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, sum_eventdays >= 10)))
psn4.2sum <- summary(glm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, sum_eventdays >= 10), family = 'poisson'))

ols4.3sum <- summary( lm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, sum_eventdays >= 25)))
psn4.3sum <- summary(glm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, sum_eventdays >= 25), family = 'poisson'))

round(ols4.1sum$coefficients[2,], 3)
round(psn4.1sum$coefficients[2,], 3)
round(exp(psn4.1sum$coefficients[2,1]), 3) 

round(ols4.2sum$coefficients[2,], 3)
round(psn4.2sum$coefficients[2,], 3)
round(exp(psn4.2sum$coefficients[2,1]), 3) 

round(ols4.3sum$coefficients[2,], 3)
round(psn4.3sum$coefficients[2,], 3)
round(exp(psn4.3sum$coefficients[2,1]), 3) 

# 5. estimate a separate effect for each park for tableau visualizations

ols5sum <- summary( lm(num_crime ~ month + parkfactor + parkfactor:anyevent, data = parkdays))
psn5sum <- summary(glm(num_crime ~ month + parkfactor + parkfactor:anyevent, data = parkdays, family = 'poisson'))

# 6. estimate separate regressions for each park class

ols6.1sum <- summary(lm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, park_class == "CITYWIDE PARK")))
ols6.2sum <- summary(lm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, park_class == "COMMUNITY PARK")))
ols6.3sum <- summary(lm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, park_class == "MAGNET PARK")))
ols6.4sum <- summary(lm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, park_class == "MINI-PARK")))
ols6.5sum <- summary(lm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, park_class == "NEIGHBORHOOD PARK")))
ols6.6sum <- summary(lm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, park_class == "PASSIVE PARK")))
ols6.7sum <- summary(lm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, park_class == "REGIONAL PARK")))

psn6.1sum <- summary(glm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, park_class == "CITYWIDE PARK"),     family = 'poisson'))
psn6.2sum <- summary(glm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, park_class == "COMMUNITY PARK"),    family = 'poisson'))
psn6.3sum <- summary(glm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, park_class == "MAGNET PARK"),       family = 'poisson'))
psn6.4sum <- summary(glm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, park_class == "MINI-PARK"),         family = 'poisson'))
psn6.5sum <- summary(glm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, park_class == "NEIGHBORHOOD PARK"), family = 'poisson'))
psn6.6sum <- summary(glm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, park_class == "PASSIVE PARK"),      family = 'poisson'))
psn6.7sum <- summary(glm(num_crime ~ anyevent + month + parkfactor, data = filter(parkdays, park_class == "REGIONAL PARK"),     family = 'poisson'))

round(ols6.1sum$coefficients[2,], 3)
round(ols6.2sum$coefficients[2,], 3)
round(ols6.3sum$coefficients[2,], 3)
round(ols6.4sum$coefficients[2,], 3)
round(ols6.5sum$coefficients[2,], 3)
round(ols6.6sum$coefficients[2,], 3)
round(ols6.7sum$coefficients[2,], 3)

round(psn6.1sum$coefficients[2,], 3); round(exp(psn6.1sum$coefficients[2,1]), 3)
round(psn6.2sum$coefficients[2,], 3); round(exp(psn6.2sum$coefficients[2,1]), 3)
round(psn6.3sum$coefficients[2,], 3); round(exp(psn6.3sum$coefficients[2,1]), 3) 
round(psn6.4sum$coefficients[2,], 3); round(exp(psn6.4sum$coefficients[2,1]), 3) 
round(psn6.5sum$coefficients[2,], 3); round(exp(psn6.5sum$coefficients[2,1]), 3) 
round(psn6.6sum$coefficients[2,], 3); round(exp(psn6.6sum$coefficients[2,1]), 3) 
round(psn6.7sum$coefficients[2,], 3); round(exp(psn6.7sum$coefficients[2,1]), 3)

# 7. estimate separate regressions for each event type

ols7.1sum  <- summary(lm(num_crime ~ anyathletic      + month + parkfactor, data = parkdays))
ols7.2sum  <- summary(lm(num_crime ~ anycomemmorative + month + parkfactor, data = parkdays))
ols7.3sum  <- summary(lm(num_crime ~ anycorporate     + month + parkfactor, data = parkdays))
ols7.4sum  <- summary(lm(num_crime ~ anyfestival      + month + parkfactor, data = parkdays))
ols7.5sum  <- summary(lm(num_crime ~ anymedia         + month + parkfactor, data = parkdays))
ols7.6sum  <- summary(lm(num_crime ~ anypicnic        + month + parkfactor, data = parkdays))
ols7.7sum  <- summary(lm(num_crime ~ anypromotion     + month + parkfactor, data = parkdays))
ols7.8sum  <- summary(lm(num_crime ~ anyatt1          + month + parkfactor, data = parkdays))
ols7.9sum  <- summary(lm(num_crime ~ anyatt5          + month + parkfactor, data = parkdays))
ols7.10sum <- summary(lm(num_crime ~ anyalcohol       + month + parkfactor, data = parkdays))

psn7.1sum  <- summary(glm(num_crime ~ anyathletic      + month + parkfactor, data = parkdays, family = 'poisson'))
psn7.2sum  <- summary(glm(num_crime ~ anycomemmorative + month + parkfactor, data = parkdays, family = 'poisson'))
psn7.3sum  <- summary(glm(num_crime ~ anycorporate     + month + parkfactor, data = parkdays, family = 'poisson'))
psn7.4sum  <- summary(glm(num_crime ~ anyfestival      + month + parkfactor, data = parkdays, family = 'poisson'))
psn7.5sum  <- summary(glm(num_crime ~ anymedia         + month + parkfactor, data = parkdays, family = 'poisson'))
psn7.6sum  <- summary(glm(num_crime ~ anypicnic        + month + parkfactor, data = parkdays, family = 'poisson'))
psn7.7sum  <- summary(glm(num_crime ~ anypromotion     + month + parkfactor, data = parkdays, family = 'poisson'))
psn7.8sum  <- summary(glm(num_crime ~ anyatt1          + month + parkfactor, data = parkdays, family = 'poisson'))
psn7.9sum  <- summary(glm(num_crime ~ anyatt5          + month + parkfactor, data = parkdays, family = 'poisson'))
psn7.10sum <- summary(glm(num_crime ~ anyalcohol       + month + parkfactor, data = parkdays, family = 'poisson'))

round(ols7.1sum$coefficients[2,],  3)
round(ols7.2sum$coefficients[2,],  3)
round(ols7.3sum$coefficients[2,],  3)
round(ols7.4sum$coefficients[2,],  3)
round(ols7.5sum$coefficients[2,],  3)
round(ols7.6sum$coefficients[2,],  3)
round(ols7.7sum$coefficients[2,],  3)
round(ols7.8sum$coefficients[2,],  3)
round(ols7.9sum$coefficients[2,],  3)
round(ols7.10sum$coefficients[2,], 3)

round(psn7.1sum$coefficients[2,],  3); round(exp(psn7.1sum$coefficients[2,1]), 3) 
round(psn7.2sum$coefficients[2,],  3); round(exp(psn7.2sum$coefficients[2,1]), 3) 
round(psn7.3sum$coefficients[2,],  3); round(exp(psn7.3sum$coefficients[2,1]), 3) 
round(psn7.4sum$coefficients[2,],  3); round(exp(psn7.4sum$coefficients[2,1]), 3) 
round(psn7.5sum$coefficients[2,],  3); round(exp(psn7.5sum$coefficients[2,1]), 3) 
round(psn7.6sum$coefficients[2,],  3); round(exp(psn7.6sum$coefficients[2,1]), 3) 
round(psn7.7sum$coefficients[2,],  3); round(exp(psn7.7sum$coefficients[2,1]), 3) 
round(psn7.8sum$coefficients[2,],  3); round(exp(psn7.8sum$coefficients[2,1]), 3) 
round(psn7.9sum$coefficients[2,],  3); round(exp(psn7.9sum$coefficients[2,1]), 3) 
round(psn7.10sum$coefficients[2,], 3); round(exp(psn7.10sum$coefficients[2,1]), 3) 

# 8. estimate separate regressions for each crime type

ols8.1sum  <- summary(lm(num_crime_property      ~ anyevent + month + parkfactor, data = parkdays))
ols8.2sum  <- summary(lm(num_crime_qualityoflife ~ anyevent + month + parkfactor, data = parkdays))
ols8.3sum  <- summary(lm(num_crime_violent       ~ anyevent + month + parkfactor, data = parkdays))

psn8.1sum  <- summary(glm(num_crime_property      ~ anyevent + month + parkfactor, data = parkdays, family = 'poisson'))
psn8.2sum  <- summary(glm(num_crime_qualityoflife ~ anyevent + month + parkfactor, data = parkdays, family = 'poisson'))
psn8.3sum  <- summary(glm(num_crime_violent       ~ anyevent + month + parkfactor, data = parkdays, family = 'poisson'))

round(ols8.1sum$coefficients[2,], 3)
round(ols8.2sum$coefficients[2,], 3)
round(ols8.3sum$coefficients[2,], 3)

round(psn8.1sum$coefficients[2,], 3); round(exp(psn8.1sum$coefficients[2,1]), 3) 
round(psn8.2sum$coefficients[2,], 3); round(exp(psn8.2sum$coefficients[2,1]), 3) 
round(psn8.3sum$coefficients[2,], 3); round(exp(psn8.3sum$coefficients[2,1]), 3) 

# 9. estimate regressions for each event class

ols9.1sum  <- summary(lm(num_crime ~ anyathletic + anycomemmorative + anycorporate + anyfestival + anymedia + 
                      anypicnic + anypromotion + month + parkfactor, data = parkdays))

psn9.1sum  <- summary(glm(num_crime ~ anyathletic + anycomemmorative + anycorporate + anyfestival + anymedia + 
                           anypicnic + anypromotion + month + parkfactor, data = parkdays, family = 'poisson'))

# merge the park-specific effects with the summary statistics and output a file for visualization
#------------------------------------------------------------------------------------------------

# find the start/end positions of the coefficients we want to store

startpos <- 12 + length(unique(parkdays$park_number))
endpos   <- startpos + length(unique(parkdays$park_number)) - 1

# store the OLS coefficients

olscoeffs <- as.data.frame(ols5sum$coefficients[startpos:endpos,])
stopifnot(dim(olscoeffs)[1] == length(unique(parkdays$park_number)))

names(olscoeffs) <- c('ols_estimate', 'ols_stderr', 'ols_tval', 'ols_pval')
olscoeffs$park_number <- str_extract(rownames(olscoeffs), 'parkfactor[0-9]+')
olscoeffs$park_number <- as.integer(str_sub(olscoeffs$park_number, 11, -1))
olscoeffs$ols_sig95   <- as.numeric(olscoeffs$ols_pval < 0.05)

head(olscoeffs, 3)
tail(olscoeffs, 3)

# store the PSN coefficients

psncoeffs <- as.data.frame(psn5sum$coefficients[startpos:endpos,])
stopifnot(dim(psncoeffs)[1] == length(unique(parkdays$park_number)))

names(psncoeffs) <- c('psn_estimate', 'psn_stderr', 'psn_zval', 'psn_pval')
psncoeffs$park_number <- str_extract(rownames(psncoeffs), 'parkfactor[0-9]+')
psncoeffs$park_number <- as.integer(str_sub(psncoeffs$park_number, 11, -1))

psncoeffs$psn_IRR       <- exp(psncoeffs$psn_estimate)
psncoeffs$psn_pctchange <- (psncoeffs$psn_IRR - 1) * 100
psncoeffs$psn_sig95     <- as.numeric(psncoeffs$psn_pval < 0.05)

head(psncoeffs, 3)
tail(psncoeffs, 3)

# merge the two estimates together with the coordinates and summary information

parkcoeffs <- inner_join(olscoeffs, psncoeffs,   by = 'park_number')
parkcoeffs <- left_join(parkcoeffs, parksummary, by = 'park_number')
parkcoeffs <- left_join(parkcoeffs, parkcoords,  by = 'park_number')
# add the park-level summary statistics and lat/long coordinates to the results

lapply(parkcoeffs, function(x) table(is.na(x)))
# 12 parks missing class/acres, 13 missing name, 1 missing coordinates

write_csv(parkcoeffs, path = 'C:/Users/elundquist/Documents/R/Output/parkcoeffs.csv')
# NOTE: write the results to CSV for visualization

