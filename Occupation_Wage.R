# This script was created to organize employment by wage classes data
# By Dongmei Chen (dchen@lcog.org)
# On August 15th, 2022

library(readxl)
library(classInt)

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

getClassfromCode <- function(x){
  # require Lane Wage Information
  df <- LaneWage_f[,c("SOC Code", "Wage Class")]
  # cls - class
  if(x %in% df$`SOC Code`){
    cls <- df[df$`SOC Code` == x,]$`Wage Class`
  }else{
    cls <- NA
  }
  return(cls)
}




############################## Update with new data ###########################################
# target tables
# 1. wage information to get wage classes
LaneWage <- read_excel(paste0(inpath, "/Lane Wage Information.xlsx"), 
                       skip = 3, n_max = 521)

names(LaneWage)[which(names(LaneWage) == "Occupation Code")] <- "SOC Code" 
# f - final
LaneWage_f <- LaneWage[,c("SOC Code", "Occupation Title", "2021 Employment*", "2022 Annual Mean (Average)**")]
names(LaneWage_f)[which(names(LaneWage_f) %in% c("2021 Employment*", "2022 Annual Mean (Average)**"))] <- c("2021 Employment","2022 Annual Mean (Average)") 
LaneWage_f <- LaneWage_f[LaneWage_f$`2022 Annual Mean (Average)` > 0, ]
LaneWage_f <- na.omit(LaneWage_f)

LaneWage_f[,"Wage Class"]  <- unlist(lapply(LaneWage_f$`2022 Annual Mean (Average)`, function(x) getClasses(x)))
LaneWage_f <- LaneWage_f[-1,]
write.csv(LaneWage_f, paste0(path, "/LaneWageInformation.csv"), row.names = FALSE)

# 2. occupation index to get percentage of employment at each wage class


readOccInd <- function(file="Lane Occ x Ind Dongmei LCOG.xlsx", sheet="110000"){
  occInd <- read_excel(paste0(inpath,"/", file), sheet = sheet)
  occInd$NAICSCode <- OccInd$NAICSCode[1]/10000
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
      df <- readOccInd(file = file, sheet = sheet)
    }else{
      ndf <- readOccInd(file = file, sheet = sheet)
      df <- rbind(df, ndf)
    }
  }
  return(df)
}

occInd <- readOccIndAll()

occInd[, "Wage Class"] <- unlist(lapply(occInd$SOCCode, function(x) getClassfromCode(x)))

# 3. Employment by sector and employment area
excel_sheets(paste0(inpath,"/Lane Census Area Dongmei LCOG.xlsx"))

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
