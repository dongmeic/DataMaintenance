# This script was created to organize low-income households and employment by wage classes data
# By Dongmei Chen (dchen@lcog.org)
# On August 4th, 2022

library(sf)
library(dplyr)

inpath <- "T:/Tableau/tableauIncome2PovRatio/Datasources"
dt <- st_read(paste0(inpath, "/BlockGroups_Embed.shp"))
head(dt)

datapath <- "T:/DCProjects/DataMaintenance/Low_Income/data"
tableID <- "B01001"
tableName <- "ACSDT5Y2016.B01001_data_with_overlays_2022-08-06T170218.csv"

readtable <- function(tableID = "B11016", 
                      year = 2016,
                      file_name_start = "ACSDT5Y", 
                      file_name_end = "2022-08-06T185245",
                      stren = 21){
  tableName <- paste0(file_name_start, year, ".", tableID, "_data_with_overlays_", file_name_end, ".csv")
  dat <- read.csv(file.path(datapath,tableID,tableName))
  dat2 <- dat[-1,-which(names(dat) %in% c("GEO_ID", "NAME"))]
  dat2 <- apply(dat2, 2, as.numeric)
  dat <- cbind(as.data.frame(dat[-1,which(names(dat)=="GEO_ID")]), as.data.frame(dat2))
  colnames(dat)[1] <- "GEO_ID"
  dat$GEO_ID <- substr(dat$GEO_ID, 10, stren)
  return(dat)
}

hhs <- readtable()
head(hhs$B11016_001E)

pov <- readtable(tableID = "C17002", 
                 year = 2016,
                 file_name_start = "ACSDT5Y", 
                 file_name_end = "2022-08-03T005947")

# hh <- readtable(tableID = "B25010", 
#                  year = 2016,
#                  file_name_start = "ACSDT5Y", 
#                  file_name_end = "2022-08-03T013233")

# remove the original data SUM_POP
#dt <- dt[,-which(names(dt)=="SUM_POP")]
st_write(dt, file.path(inpath, "BlockGroups_Embed.shp"), delete_layer = TRUE)


