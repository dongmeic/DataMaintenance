# This script was created to organize low-income households data
# By Dongmei Chen (dchen@lcog.org)
# On August 4th, 2022

library(sf)

inpath <- "T:/Tableau/tableauIncome2PovRatio/Datasources"
# o - old
odt <- st_read(paste0(inpath, "/BlockGroups_AllYears.shp"))
head(odt)
bg_ltd <- st_read(paste0(inpath,"/BG_clipped.shp"))

YEAR <- 2020
bglyr <- st_read(dsn = "T:/DCProjects/DataMaintenance/Low_Income/Low_Income.gdb",
                layer = paste0("tl_", YEAR, "_41_bg_Lane"))
dt <- bglyr[,"GEOID"]
colnames(dt)[1] <- "GEO_ID"
  
datapath <- "T:/DCProjects/DataMaintenance/Low_Income/data"

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

bglyr <- st_transform(bglyr, st_crs(bg_ltd))
ltd_in_bg <- st_intersection(bglyr, bg_ltd)
class(ltd_in_bg)
plot(ltd_in_bg$Shape)

pov$PopLessTha <- pov$C17002_001E - pov$C17002_008E
pov$Population <- pov$C17002_001E
dt <- merge(dt, pov[,c("GEO_ID", "PopLessTha", "Population")],  by="GEO_ID")
dt$LTD <- ifelse(dt$GEO_ID %in% unique(ltd_in_bg$GEOID), "Y", "N")
hhs$TotalHHLDs <- hhs$B11016_001E
dt <- merge(dt, hhs[,c("GEO_ID", "TotalHHLDs")], by="GEO_ID")
colnames(dt)[1] <- "BlockGro_1"
dt$Year <- YEAR
dt <- dt[,c("BlockGro_1", "PopLessTha", "Population", "LTD", "TotalHHLDs", "Year", "geometry")]
st_write(dt, file.path(inpath, "BlockGroups_Embed.shp"), delete_layer = TRUE)
dt <- rbind(odt, dt)
st_write(dt, file.path(inpath, "BlockGroups_AllYears.shp"), delete_layer = TRUE)

# state (or) level
YEAR <- 2016
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
# state average level (used for the state average reference line)
(c*(a-b)/a)/c
# 2020 - 0.2928479
# 2018 - 0.3251808
# 2016 - 0.3521265


dt$pctlow <- (dt$TotalHHLDs * (dt$PopLessTha / dt$Population))/dt$TotalHHLDs

for(yr in unique(dt$Year)){
  print(yr)
  t <- dim(dt[dt$Year==yr & dt$TotalHHLDs != 0,])[1]
  print(paste("No. records:", t))
  k <- t/10
  pctL <- dt[dt$Year == yr,]$pctlow
  top_pct <- sort(pctL)[c(t-k, t-k*2, t-k*3, t-k*4, t-k*5, t-k*6, t-k*7)]
  print(top_pct)
}

# [1] 2016
# [1] "No. records: 256"
# [1] 0.6576338 0.5594578 0.4945455 0.4331723 0.3756614 0.3313856 0.2922636
# [1] 2018
# [1] "No. records: 256"
# [1] 0.6012270 0.5238959 0.4576360 0.4129663 0.3465347 0.3014354 0.2615741
# [1] 2020
# [1] "No. records: 270"
# [1] 0.5919067 0.5019506 0.4520440 0.3775727 0.3193473 0.2730242 0.2276029


# test for 2020 data
# totally 270 features (with one zero-hh bg)
t <- 270
# this will show the top percents for the block group parameter
para_val <- c(27, 27*2, 27*3, 27*4, 27*5, 27*6, 27*7)
top_pct <- sort(pctlow)[c(t-27, t-27*2, t-27*3, t-27*4, t-27*5, t-27*6, t-27*7)]

df <- data.frame(bg=dt$BlockGro_1, pct=pctlow)
df[df$pct %in% top_pct, "bg"]
