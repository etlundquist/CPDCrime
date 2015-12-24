# set enviornment variables and load required packages
#-----------------------------------------------------

library(RPostgreSQL)
library(dplyr)
library(lubridate)

setwd('C:/Users/elundquist/Documents/R')
source('./Authentication/gpcnx.r')

cnx <- dbConnect(dbDriver("PostgreSQL"),
                 host     = gp.host, 
                 port     = gp.port, 
                 dbname   = gp.dbname, 
                 user     = gp.user, 
                 password = gp.password)

dbSendQuery(cnx, 'SET search_path TO cpd')
# NOTE: all relevant tables are on the CPD schema

# pull the [lat/long] coordinates for every park into a data file to use later
#-----------------------------------------------------------------------------

parkcoords <- dbGetQuery(cnx, 'SELECT park_number, latitude, longitude FROM parks')
str(parkcoords); lapply(parkcoords, function(x) table(is.na(x)))

parkcoords <- filter(parkcoords, !is.na(latitude) & !is.na(longitude))
# NOTE: 4 parks have missing coordinates and therefore won't be used for the time being

# pull the weather for every park/day to merge into the analysis file
#--------------------------------------------------------------------

parkweather <- dbGetQuery(cnx, 'SELECT park_number, obs_date, humidityavg, windspeedavgmph, temperatureavgf, precipitationsumin FROM park_weather')
str(parkweather); lapply(parkweather, function(x) table(is.na(x)))

nrow(parkweather); nrow(distinct(select(parkweather, park_number, obs_date)))
group_by(parkweather, park_number, obs_date) %>% filter(n() > 1) %>% select(park_number, obs_date) %>% arrange(park_number, obs_date)
parkweather <- distinct(parkweather, park_number, obs_date)
# NOTE: remove park/date duplicates in the weather data - this screws up the merge to the analysis file

length(unique(parkweather$park_number)); summary(parkweather$obs_date)
names(parkweather) <- c('park_number', 'date', 'humid', 'wind', 'temp', 'precip')

parkweather$month <- month(parkweather$date)
monthweather <- group_by(parkweather, month) %>% summarize_each(funs(mean, min, max), humid:precip)

select(monthweather, starts_with('humid'))  # relative humidity as a percent [0,100] - averages seem reasonable
select(monthweather, starts_with('wind'))   # average wind speed looks a bit low to me (ORLY windy city?)
select(monthweather, starts_with('temp'))   # averages seem reasonable - some of the highs/lows look off 
select(monthweather, starts_with('precip')) # there's more rain in the summer - who knew?

parkweather <- select(parkweather, park_number, date, temp, precip)
length(parkweather$precip[parkweather$precip > 0])/length(parkweather$precip)

# bring in the main analysis file and do some initial validation/variable creation
#---------------------------------------------------------------------------------

parkdays <- postgresqlReadTable(cnx, 'aggregate_data', row.names = NULL)
str(parkdays); lapply(parkdays, function(x) table(is.na(x)))
# NOTE: some parks missing [CLASS/ACRES/NAME/ZIP/INCOMEBIN]

stopifnot(nrow(parkdays) == nrow(distinct(parkdays, date, park_number))) # check ID level of file
summary(group_by(parkdays, park_number) %>% summarize(numdays = n()))    # check rectangularization
summary(parkdays$date)                                                   # check date range of file

eventparks <- distinct(filter(parkdays, num_events > 0) %>% select(park_number))
crimeparks <- distinct(filter(parkdays, num_crime  > 0) %>% select(park_number))

nrow(eventparks); nrow(crimeparks); nrow(inner_join(eventparks, crimeparks, by = 'park_number'))
# NOTE: 261 parks with at least one event, 582 parks with at least one crime, 253 with one of both

eventparks$everevent <- TRUE
crimeparks$evercrime <- TRUE

parkdays <- left_join(parkdays, eventparks, by = 'park_number')
parkdays <- left_join(parkdays, crimeparks, by = 'park_number')
rm(crimeparks, eventparks)

parkdays$everevent[is.na(parkdays$everevent)] <- FALSE
parkdays$evercrime[is.na(parkdays$evercrime)] <- FALSE
summary(select(parkdays, park_number, everevent, evercrime) %>% distinct(park_number))
# NOTE: add the "any" binaries back to the main park/day data

parkdays <- filter(parkdays, everevent == TRUE)
# NOTE: restrict the data to only parks that held at least one event during the data period
# NOTE: this is necessary to examine the change in crime rate on event days vs. non-event days

parkdays$anyevent          <- as.numeric(parkdays$num_events          > 0)
parkdays$anyathletic       <- as.numeric(parkdays$num_athletic        > 0)
parkdays$anycomemmorative  <- as.numeric(parkdays$num_comemmorative   > 0)
parkdays$anycorporate      <- as.numeric(parkdays$num_corporate       > 0)
parkdays$anyfestival       <- as.numeric(parkdays$num_festival        > 0)
parkdays$anymedia          <- as.numeric(parkdays$num_media           > 0)
parkdays$anypicnic         <- as.numeric(parkdays$num_picnic          > 0)
parkdays$anypromotion      <- as.numeric(parkdays$num_promotion       > 0)
parkdays$anyatt1           <- as.numeric(parkdays$num_attendance_cat1 > 0)
parkdays$anyatt5           <- as.numeric(parkdays$num_attendance_cat5 > 0)
parkdays$anyalcohol        <- as.numeric(parkdays$num_alcohol_events  > 0)

parkdays$wday  <- wday(parkdays$date)
parkdays$month <- month(parkdays$date)
parkdays$wday  <- factor(parkdays$wday,  labels = c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'))
parkdays$month <- factor(parkdays$month, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
parkdays$parkfactor <- as.factor(parkdays$park_number)
# NOTE: create factor variables for [weekday, month, park_number]

parkdays <- left_join(parkdays, parkweather, by = c('park_number', 'date'))
unique(parkdays$park_number[is.na(parkdays$temp) | is.na(parkdays$precip)])
# NOTE: Park 255 isn't found in the weather data for some reason

length(parkdays$precip[parkdays$precip > 0])/length(parkdays$precip)
parkdays$anyprecip <- as.numeric(parkdays$precip > 0)
# NOTE: 29% of park/days in the analysis sample had any precipitation - code this as a binary for analysis

# create summary statistics at the park level to eventually merge into results
#-----------------------------------------------------------------------------

parksummary <- group_by(parkdays, park_number) %>% 
summarize(class         = first(park_class), acres             = first(acres),   
          name          = first(park_name),  zip               = first(zip),        
          incomebin     = first(incomeBin),  mean_crime        = mean(num_crime),   
          median_crime  = median(num_crime), sum_crime         = sum(num_crime),
          sum_events    = sum(num_events),   sum_eventdays     = sum(anyevent),
          everathletic  = max(anyathletic),  evercomemmorative = max(anycomemmorative),  
          evercorporate = max(anycorporate), everfestival      = max(anyfestival),
          evermedia     = max(anymedia),     everpicnic        = max(anypicnic),
          everpromotion = max(anypromotion), everatt1          = max(anyatt1),
          everatt5      = max(anyatt5),      everalcohol       = max(anyalcohol))

length(parksummary$sum_eventdays[parksummary$sum_eventdays >=  5]) # 148
length(parksummary$sum_eventdays[parksummary$sum_eventdays >= 10]) # 95
length(parksummary$sum_eventdays[parksummary$sum_eventdays >= 25]) # 57
sapply(select(parksummary, starts_with('ever')), sum)

parkevents <- select(parksummary, park_number, sum_eventdays, starts_with('ever'))
parkdays   <- left_join(parkdays, parkevents, by = 'park_number')

# save the coordinates, summary, and analysis files for use in modeling
#----------------------------------------------------------------------

save(parkcoords,  file = './Rdata/CPDCoordinatesFile.rdata')
save(parkdays,    file = './Rdata/CPDAnalysisFile.rdata')
save(parksummary, file = './Rdata/CPDSummaryFile.rdata')
# save the analysis file to load in later or use via the linux cluster
