# This script was created to organize low-income households and employment by wage classes data
# By Dongmei Chen (dchen@lcog.org)
# On August 4th, 2022

library(sf)


inpath <- "T:/Tableau/tableauIncome2PovRatio/Datasources"
# o - old
odt <- st_read(paste0(inpath, "/BlockGroups_Embed.shp"))
head(odt)
bg_ltd <- st_read(paste0(inpath,"/BG_clipped.shp"))
bg20 <- st_read(dsn = "T:/DCProjects/DataMaintenance/Low_Income/Low_Income.gdb",
                layer = "tl_2020_41_bg_Lane")
dt <- bg20[,"GEOID"]
colnames(dt)[1] <- "GEO_ID"
  
datapath <- "T:/DCProjects/DataMaintenance/Low_Income/data"
tableID <- "B01001"
tableName <- "ACSDT5Y2016.B01001_data_with_overlays_2022-08-06T170218.csv"

readtable <- function(tableID = "B11016", 
                      year = 2016,
                      file_name_start = "ACSDT5Y", 
                      file_name_end = "2022-08-06T185245",
                      stren = 21,
                      state = FALSE){
  tableName <- paste0(file_name_start, year, ".", tableID, "_data_with_overlays_", file_name_end, ".csv")
  if(state){
    datapath <- paste0(datapath, "/", tableID, "/state")
    dat <- read.csv(file.path(datapath,tableName))
    dat2 <- dat[-1,-which(names(dat) %in% c("GEO_ID", "NAME"))]
    dat2 <- apply(dat2, 2, as.numeric)
    dat <- cbind(data.frame(Variable=names(dat2)), data.frame(Value=dat2))
    rownames(dat) <- NULL
  }else{
    dat <- read.csv(file.path(datapath,tableID,tableName))
    dat2 <- dat[-1,-which(names(dat) %in% c("GEO_ID", "NAME"))]
    dat2 <- apply(dat2, 2, as.numeric)
    dat <- cbind(as.data.frame(dat[-1,which(names(dat)=="GEO_ID")]), as.data.frame(dat2))
    colnames(dat)[1] <- "GEO_ID"
    dat$GEO_ID <- substr(dat$GEO_ID, 10, stren)
  }
  return(dat)
}

#YEAR <- 2016
YEAR <- 2020
hhs <- readtable(year = YEAR)
head(hhs$B11016_001E)

pov <- readtable(tableID = "C17002", 
                 year = YEAR,
                 file_name_start = "ACSDT5Y", 
                 file_name_end = "2022-08-03T005947")

# hh <- readtable(tableID = "B25010", 
#                  year = 2016,
#                  file_name_start = "ACSDT5Y", 
#                  file_name_end = "2022-08-03T013233")

# remove the original data SUM_POP
#odt <- odt[,-which(names(odt)=="SUM_POP")]

bg20 <- st_transform(bg20, st_crs(bg_ltd))
ltd_in_bg <- st_intersection(bg20, bg_ltd)
class(ltd_in_bg)
plot(ltd_in_bg$Shape)

pov$PopLessTha <- pov$C17002_001E - pov$C17002_008E
pov$Population <- pov$C17002_001E
dt <- merge(dt, pov[,c("GEO_ID", "PopLessTha", "Population")],  by="GEO_ID")
dt$LTD <- ifelse(dt$GEO_ID %in% unique(ltd_in_bg$GEOID), "Y", "N")
hhs$TotalHHLDs <- hhs$B11016_001E
dt <- merge(dt, hhs[,c("GEO_ID", "TotalHHLDs")], by="GEO_ID")
colnames(dt)[1] <- "BlockGro_1"
st_write(dt, file.path(inpath, "BlockGroups_Embed.shp"), delete_layer = TRUE)

# o - old
o_pctlow <- (odt$TotalHHLDs*(odt$PopLessTha/odt$Population))/odt$TotalHHLDs
quantile(o_pctlow, c(.1, .2, .3, .4, .5, .6, .7), na.rm = T)

pctlow <- (dt$TotalHHLDs*(dt$PopLessTha/dt$Population))/dt$TotalHHLDs
quantile(pctlow, c(.1, .2, .3, .4, .5, .6, .7), na.rm = T)

# state (or) level
or_pov <- readtable(tableID = "C17002", 
                    year = YEAR,
                    file_name_end = "2022-08-08T041957",
                    state = TRUE)

or_hhs <- readtable(year = YEAR,
                    file_name_end = "2022-08-08T042141",
                    state = TRUE)

a <- or_pov[or_pov$Variable=="C17002_001E", "Value"]
b <- or_pov[or_pov$Variable=="C17002_008E", "Value"]
c <- or_hhs[or_hhs$Variable=="B11016_001E", "Value"]
# state average level
(c*(a-b)/a)/c

# totally 270 features (with one zero-hh bg)
t <- 270
top_pct <- sort(pctlow)[c(t-27, t-27*2, t-27*3, t-27*4, t-27*5, t-27*6, t-27*7)]

df <- data.frame(bg=dt$BlockGro_1, pct=pctlow)
df[df$pct %in% top_pct, "bg"]
