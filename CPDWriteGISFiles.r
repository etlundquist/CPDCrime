setwd('C:/Users/elundquist/Documents')
source('./R/Authentication/gpcnx.r')
library(RPostgreSQL)
library(readr)

cnx <- dbConnect(dbDriver("PostgreSQL"),
       host     = gp.host, 
       port     = gp.port, 
       dbname   = 'scratch', 
       user     = gp.user, 
       password = gp.password)

dbSendQuery(cnx, 'SET search_path TO cpd')

AttributedCrimes <- read_csv('./Data/AttributedCrimes.csv', col_names = TRUE)
ParkBuffers      <- read_csv('./Data/ParkBuffers.csv', col_names = TRUE)

postgresqlWriteTable(cnx, 'attributed_crimes', AttributedCrimes, row.names = FALSE, overwrite = TRUE)
postgresqlWriteTable(cnx, 'park_buffers',      ParkBuffers,      row.names = FALSE, overwrite = TRUE)

