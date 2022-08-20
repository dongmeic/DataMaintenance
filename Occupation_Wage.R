# This script was created to organize employment by wage classes data
# By Dongmei Chen (dchen@lcog.org)
# On August 15th, 2022

library(readxl)
library(classInt)
library(tidyr)
library(sf)

inpath <- "T:/DCProjects/DataMaintenance/Low_Income/data"
path <- "T:/Tableau/tableauIncome2PovRatio/Datasources"

############################## functions ##########################################################
getClassRange <- function(x){
  text1 <- str_replace_all(x, "\\[", "")
  text2 <- str_replace_all(text1, "\\)|]", "")
  minv <- as.numeric(str_split(text2, ",")[[1]][1])
  maxv <- as.numeric(str_split(text2, ",")[[1]][2])
  return(c(minv, maxv))
}

getClasses <- function(x, colnm = "2022 Annual Mean (Average)", df=LaneWage_f){
  names(df)[which(names(df)==colnm)] <- "annmean"
  wageclasses <- levels(classify_intervals(df$annmean, 5, style = "quantile"))
  classnames <- c("Very Low", "Low", "Medium", "High", "Very High")
  for(class in wageclasses){
    i <- which(wageclasses == class)
    minv <- getClassRange(class)[1]
    maxv <- getClassRange(class)[2]
    if(x >= minv & x <= maxv){
      return(classnames[i])
    }
  }
}

getClassfromCode <- function(x, df=LaneWage_f){
  # require Lane Wage Information
  df <- df[,c("SOC Code", "Wage Class")]
  # cls - class
  if(x %in% df$`SOC Code`){
    cls <- df[df$`SOC Code` == x,]$`Wage Class`
  }else{
    cls <- NA
  }
  return(cls)
}

readOccInd <- function(file="Lane Occ x Ind Dongmei LCOG.xlsx", sheetnm="110000"){
  occInd <- read_excel(paste0(inpath,"/", file), sheet = sheetnm)
  occInd$NAICSCode <- occInd$NAICSCode[1]/10000
  occInd$NAICSTitle <- occInd$NAICSTitle[1]
  occInd <- occInd[-1,]
  occInd1 <- occInd[occInd$SOCCode != "Other",]
  occInd2 <- occInd[occInd$SOCCode == "Other",]
  occInd1$SOCCode <- unlist(lapply(occInd1$SOCCode, function(x) paste0(substr(x, 1, 2), "-", str_sub(x, -4, -1))))
  occInd_f <- rbind(occInd1, occInd2)
  return(occInd_f)
}

readOccIndAll <- function(file="Lane Occ x Ind Dongmei LCOG.xlsx"){
  sheets <- excel_sheets(paste0(inpath,"/", file))
  sheets <- sheets[-which(sheets=="Sheet1")]
  for(sheet in sheets){
    print(sheet)
    if(which(sheets==sheet)==1){
      df <- readOccInd(file = file, sheetnm = sheet)
    }else{
      ndf <- readOccInd(file = file, sheetnm = sheet)
      df <- rbind(df, ndf)
    }
  }
  return(df)
}

getPctTable <- function(df=occInd){
  a <- df[,c("NAICSCode", "Employment", "Wage Class")]
  names(a)[3] <- "Class"
  b <- aggregate(Employment~NAICSCode + Class, data=a, FUN = "sum")
  c <- b %>% 
    spread(Class, Employment) %>% 
    replace(is.na(.), 0) %>% 
    mutate(sum=rowSums(.[2:6]))
  d <- cbind(c[,"NAICSCode"], as.data.frame(sapply(c[,2:6], function(x) x/c$sum)))
  names(d)[1] <- "NAICSCode"
  d <- d[,c("NAICSCode", "Very High", "High", "Medium", "Low", "Very Low")]
  return(d)
}

readCensusAreaEmp <- function(file = "Lane Census Area Dongmei LCOG.xlsx", 
                              sheetnm = "410390001", other=FALSE){
  CAemp <- read_excel(paste0(inpath,"/", file), sheet = sheetnm, skip=5)
  CAemp <- CAemp[-1,]
  CAemp$EmpArea <- unlist(lapply(CAemp$`Confidentiality / Industry`, function(x) str_split(x, " ")[[1]][1]))
  if(other){
    CAemp1 <- CAemp[CAemp$EmpArea == "Other",]
    CAemp1$EmpArea <- CAemp$EmpArea[1]
    CAemp1$Sector <- 0
    CAemp1$Industry <- "Other"
    CAemp <- CAemp1
  }else{
    CAemp$Sector <- unlist(lapply(CAemp$`Confidentiality / Industry`, function(x) str_split(x, " ")[[1]][2]))
    CAemp$Industry <- paste(str_split(CAemp$`Confidentiality / Industry`[1], " ")[[1]][-2:-1], collapse = " ")
  }
  names(CAemp)[c(3, 5)] <- c("Emp", "AvgPay")
  CAemp <- CAemp[,c("EmpArea", "Sector", "Industry", "Estab", "Emp", "TotPay", "AvgPay")]
  return(CAemp)
}

readCensusAreaEmpAll <- function(file = "Lane Census Area Dongmei LCOG.xlsx", other=FALSE){
  sheets <- excel_sheets(paste0(inpath,"/", file))
  sheets <- sheets[-which(sheets=="Lane County")]
  for(sheet in sheets){
    print(sheet)
    if(which(sheets==sheet)==1){
      if(other){
        df <- readCensusAreaEmp(file = file, sheetnm = sheet, other=TRUE)
      }else{
        df <- readCensusAreaEmp(file = file, sheetnm = sheet)
      }
    }else{
      if(other){
        ndf <- readCensusAreaEmp(file = file, sheetnm = sheet, other=TRUE)
      }else{
        ndf <- readCensusAreaEmp(file = file, sheetnm = sheet)
      }
      df <- rbind(df, ndf)
    }
  }
  return(df)
}

getEmpbyClass <- function(x=74, code=11, class="Very High"){
  rate <- Pct_df[Pct_df$NAICSCode==code, class]
  return(x*rate)
}

############################## Update with new data ###########################################
# target tables
# 1. wage information to get wage classes
LaneWage <- read_excel(paste0(inpath, "/Lane Wage Information.xlsx"), sheet = "Classified",n_max = 473)

names(LaneWage)[which(names(LaneWage) == "Occupation Code")] <- "SOC Code" 
# f - final
LaneWage_f <- LaneWage[,c("SOC Code", "Occupation Title", "2021 Employment*", "2022 Annual Mean (Average)**", "Wage Class")]
names(LaneWage_f)[which(names(LaneWage_f) %in% c("2021 Employment*", "2022 Annual Mean (Average)**"))] <- c("2021 Employment","2022 Annual Mean (Average)") 
LaneWage_f <- LaneWage_f[LaneWage_f$`2022 Annual Mean (Average)` > 0, ]
LaneWage_f <- na.omit(LaneWage_f)
LaneWage_f <- LaneWage_f[-1,]
#LaneWage_f[,"Wage Class"]  <- unlist(lapply(LaneWage_f$`2022 Annual Mean (Average)`, function(x) getClasses(x)))
# levels(classify_intervals(LaneWage_f$`2022 Annual Mean (Average)`, 5, style = "quantile"))
# #[1] "[29673,39403.4)"   "[39403.4,48207.4)" "[48207.4,61643.4)" "[61643.4,78020.4)" "[78020.4,288125]" 

write.csv(LaneWage_f, paste0(path, "/LaneWageInformation.csv"), row.names = FALSE)

# 2. occupation index to get percentage of employment at each wage class
occInd <- readOccIndAll()
occInd[, "Wage Class"] <- unlist(lapply(occInd$SOCCode, function(x) getClassfromCode(x)))
names(occInd)[which(names(occInd)=="Employment\r\n2020")] <- "Employment"

Pct_df <- getPctTable()

# 3. Employment by sector and employment area
df <- readCensusAreaEmpAll()
for(code in unique(df$Sector)){if(!(code %in% unique(Pct_df$NAICSCode))){print(code)}}
df <- df[!is.na(df$Sector) & df$Sector != "22",]
df_other <- readCensusAreaEmpAll(other=TRUE)

df$VeryHigh <- unlist(mapply(getEmpbyClass, df$Emp, df$Sector, "Very High"))
df$High <- unlist(mapply(getEmpbyClass, df$Emp, df$Sector, "High"))
df$Medium <- unlist(mapply(getEmpbyClass, df$Emp, df$Sector, "Medium"))
df$Low <- unlist(mapply(getEmpbyClass, df$Emp, df$Sector, "Low"))
df$VeryLow <- unlist(mapply(getEmpbyClass, df$Emp, df$Sector, "Very Low"))

df_other$VeryHigh <- 0
df_other$High <- 0
df_other$Medium <- 0
df_other$Low <- 0
df_other$VeryLow <- 0
df_other$NoData <- df_other$Emp

df$NoData <- 0
df <- rbind(df, df_other)
write.csv(df, paste0(path, "/SumByAreaXSector.csv"), row.names = FALSE)

totemp <- st_read(paste0(path, "/EmpAreas.shp"))
totemp <- totemp[,-which(names(totemp)=="TotalEmp")]
totempdf <- aggregate(Emp~EmpArea, data=df, FUN = "sum")
totemp <- merge(totemp, totempdf, by="EmpArea")
names(totemp)[which(names(totemp)=="Emp")] <- "TotalEmp"
st_write(totemp, paste0(path, "/EmpAreas.shp"), delete_layer = TRUE)


############################## Review old data ###############################################
# ea - employment area
ea <- read_excel(paste0(path, "/EmploymentByArea.xlsx"), sheet = "EmpAreaWageClass4Tableau")
# final table format
sum_by_area <- read_excel(paste0(path, "/EmploymentByArea.xlsx"), sheet = "SumByAreaXSector")
wage <- read_excel(paste0(inpath, "/Lane Wage Information 2018.xlsx"), sheet = "SOCForTableau")

occu <- read_excel(paste0(inpath, "/LaneOccXInd 2018.xlsx"), sheet = "LaneOccXInd")

# check the wage classes
a <- wage[wage$`Wage Class`=='Very High',]$`SOC Code`
b <- occu[occu$`Wage Class...6` == "Very High",]$SOCCode

setdiff(a, b)
length(setdiff(a, b))
length(intersect(a, b))

names(occu)

classes <- unique(wage$`Wage Class`)
data <- wage$`2018 Annual Mean (Average)`

for(class in classes){
  print(paste("Wage classes:", class))
  print(paste(range(wage[wage$`Wage Class`== class,]$`2018 Annual Mean (Average)`), collapse = " - "))
}

 # "Wage classes: Very High"
 # "61029 - 244965"
 # "Wage classes: High"
 # "42041 - 60829"
 # "Wage classes: Medium"
 # "34043 - 41950"
 # "Wage classes: Low"
 # "29166 - 33664"
 # "Wage classes: Very Low"
 # "24193 - 28859"

levels(classify_intervals(data, 5, style = "quantile"))
levels(classify_intervals(data, 5, style = "equal"))
levels(classify_intervals(data, 5, style = "jenks"))
levels(classify_intervals(data, 5, style = "kmeans"))
