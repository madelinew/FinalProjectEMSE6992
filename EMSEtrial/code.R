library(readxl)
X2016txttooExcel <- read_excel("~/Desktop/EMSE 6992 Portfolio/FInal Project/FinalProjectEMSE6992/EMSEtrial/Data/2016txttooExcel.xls",
                               col_types = c("numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                             "date", "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric","numeric"))
FARS_16 <- as.data.frame(X2016txttooExcel, header = TRUE, as.is=TRUE)

library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("knitr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("gridExtra", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("png", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("pander", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
# SETTING UP DATAFRAME FROM EXCEL DATAFRAME ------------------------------
# df = FARS_YY dataframe (df) (ex: FARS_16) 
# YYYY = case year of the Excel dataframe (ex: 2016)
fatal_YYnasetup <- function(df, YYYY) {
  #df = FARS_YY
  #YYYY = case year
  y = YYYY
  fataldf <- as.data.frame(cbind.data.frame(df$casenum, df$statenum, df$vnumber, df$pnumber, df$city, df$county, df$latitude,
                                            df$longitude, df$age, df$vfatcount, df$accday, df$accmon,
                                            df$caseyear,df$accdate, df$modelyr, df$make, 
                                            df$dridistract1, df$dridistract2, df$dridistract3), 
                           header = TRUE, as.is = TRUE, stringsAsFactor=FALSE)
  colnames(fataldf) <- c("casenum", "statenum", "vehnum","pernum", "citynum", "countynum", "lat", "long", "age", "numfatalveh", "day", "month", 
                         "year","date", "modelyr", "make", "dridistract1", "dridistract2", "dridistract3")
  #START Getting Date to be a Date
  fataldf$date <- df$accdate
  fataldf$date <-format.POSIXct(fataldf$date, format = "%m/%Y")
  #END
  #Fixing city and county as num
  fataldf$citynum <- as.integer(fataldf$citynum)
  fataldf$countynum <- as.integer(fataldf$countynum)
  fataldf$lat <- as.character(fataldf$lat)
  fataldf$long <- as.character(fataldf$long)
  fataldf$age <- as.integer(fataldf$age)
  fataldf <- unique(fataldf)
  #removes error coded ages and sets them to NA so that the person is still counted
  #998 code in age means that it wasn't indicated
  #999 code in age means that it was unknown
  fataldf$age <- na_if(fataldf$age, 998)
  fataldf$age <- na_if(fataldf$age, 999)
  
  return(fataldf)
}

fatal_YYsetup <- function(df, YYYY) {
  #df = FARS_YY
  #YYYY = case year
  y = YYYY
  fataldf <- as.data.frame(cbind.data.frame(df$casenum, df$statenum, df$vnumber, df$pnumber, df$city, df$county, df$latitude,
                                            df$longitude, df$age, df$vfatcount, df$accday, df$accmon,
                                            df$caseyear,df$accdate, df$modelyr, df$make, 
                                            df$dridistract1, df$dridistract2, df$dridistract3), 
                           header = TRUE, as.is = TRUE, stringsAsFactor=FALSE)
  colnames(fataldf) <- c("casenum", "statenum", "vehnum","pernum", "citynum", "countynum", "lat", "long", "age", "numfatalveh", "day", "month", 
                         "year","date", "modelyr", "make", "dridistract1", "dridistract2", "dridistract3")
  #START Getting Date to be a Date
  fataldf$date <- df$accdate
  fataldf$date <-format.POSIXct(fataldf$date, format = "%m/%Y")
  #END
  #Fixing city and county as num
  fataldf$citynum <- as.integer(fataldf$citynum)
  fataldf$countynum <- as.integer(fataldf$countynum)
  fataldf$lat <- as.character(fataldf$lat)
  fataldf$long <- as.character(fataldf$long)
  fataldf$age <- as.integer(fataldf$age)
  fataldf <- unique(fataldf)
  #removes error coded ages and sets them to NA so that the person is still counted
  fataldf <- fataldf[fataldf$age < 997,]
  
  return(fataldf)
}

## STORE RESULT OF FUNCTION IN MAIN DATA FRAME ##
#INPUT EXAMPLE:   fatal_16 <- fatal_YYsetup(FARS_16, 2016) #
fatal_16na <- fatal_YYnasetup(FARS_16, 2016)
fatal_16 <- fatal_YYsetup(FARS_16, 2016)


# FINDING CELL CASES ---------------------------------
#Function for returning distracted driving subdataframe with cellphoneuse as a binomial indicator 
#INPUT: df = fatal_YY YY = case year
#OUTPUT: fataldisdf
findingCellCases <- function(df, YY) {
  #df = fatal_YY
  #YY = case year
  setupfataldis <- function(df) {
    setupdrf <- function(df){
      #5 = Talking or listening to cell phone
      #6 = While dialing Cell phone
      #15 = Other cell phone distraction
      df$cellphoneuse <- ifelse(df$dridistract1 == 5, 1,
                                ifelse(df$dridistract2 == 5, 1,
                                       ifelse(df$dridistract3 == 5, 1,
                                              ifelse(df$dridistract1 == 6, 1,
                                                     ifelse(df$dridistract2 == 6, 1,
                                                            ifelse(df$dridistract3 == 6, 1,
                                                                   ifelse(df$dridistract1 == 15, 1,
                                                                          ifelse(df$dridistract2 == 15, 1,
                                                                                 ifelse(df$dridistract3 == 15, 1, 0)))))))))
      drop <- c("citynum","countynum","lat","long","dridistract1", "dridistract2","dridistract3")
      df = df[,!(names(df) %in% drop)]
      return(df)
    }
    fataldisYY <- setupdrf(df)
    return(fataldisYY)
  }
  fataldisYY <- setupfataldis(df)
  return(fataldisYY)
}



fatal_16nads <- findingCellCases(fatal_16na, 16)
fatal_16ds <- findingCellCases(fatal_16, 16)


# Setting Dataset ---------------------------------------------------------
#create dataset to be used with casenum, statenum, vehnum, persontotal (total number of people in car), 
#     agemin (minimum age of person in car), agemax (max age of person in car), numfatal (number of fatalities in vehicle),
#     day, month, year, date, modelyear, make, cellphoneuse

#Function for labeling which cases had duplicate entries (due to mulitple vehicles or multiple people) 
#INPUT: df = fatal_YYds
#OUTPUT: fatal_YYds with duplicate label column
findingDuplicateCases <- function(df) {
  #df = casedf
  ndf <- df
  ndf$duplicate <- "no"
  lengthcases <- length(df$casenum)
  rownames(df) <- c(1:length(df$casenum))
  i = 1
  for (w in 2:lengthcases) {
    testrow <- ndf[i,]
    if (w != i){
      comparerow <- df[w,]
      if (testrow[1,1] == comparerow[1,1]) {
        if(testrow[1,2] == comparerow[1,2]){
          ndf[i,]$duplicate <- "yes"
          ndf[w,]$duplicate <- "yes"
        } else {
          if (ndf[i,]$duplicate == "yes") {
            ndf[i,]$duplicate == "yes"
          } else {
            ndf[i,]$duplicate <- "no"
          }
        }
      } else {
        if (ndf[i,]$duplicate == "yes") {
          ndf[i,]$duplicate == "yes"
        } else {
          ndf[i,]$duplicate <- "no"
        }
      }
    } else {
      next
    }
    i = i + 1
  } #close for loop
  return(ndf)
}

fatal_16nads <- findingDuplicateCases(fatal_16nads)
fatal_16ds <- findingDuplicateCases(fatal_16ds)


#Function for returning data frame from fatal_16ds that were duplicates 
#INPUT: df = fatal_YYds 
#OUTPUT: ysdf (data frame from "yes")
yesnadf <- function(df) {
  ydf <- df[df$duplicate == "yes",]
  drop <- c("duplicate","pernum")
  ydf = ydf[,!(names(ydf) %in% drop)]
  
  ydf$casestateveh <- paste(ydf$casenum,ydf$statenum,ydf$vehnum, sep = "")
  ndf <- data.frame(casenum = numeric(), statenum = numeric(), vehnum = numeric(), persontotal = numeric(),
                    agemin = numeric(), agemax = numeric(),
                    numfatal = numeric(), day = numeric(), month = numeric(),
                    year = numeric(), date = character(), modelyear = numeric(), make = numeric(), cellphoneuse = numeric())
  
  caselist <- unique(ydf$casestateveh)
  for (i in caselist) {
    
    cdf <- ydf[ydf$casestateveh == i,]
    
    pt <- length(cdf$casenum)
    
    cr <- cdf
    drop <- "age"
    cr = cr[,!(names(cr) %in% drop)]
    cr <- unique(cr)
    cdf <- cdf[!is.na(cdf$age),]
    if (length(cdf$age) == 0){
      minage <- NA
      maxage <- NA
    } else {
      if (length(cdf$age) > 0 & length(cdf$age) < 2) {
        minage <- cdf$age
        maxage <- cdf$age
      }  else {
        minage <- min(cdf$age)
        maxage <- max(cdf$age)
      }
    }
    
    ndf <- rbind(ndf, data.frame(casenum = cr[1,1], statenum = cr[1,2], vehnum = cr[1,3], 
                                 persontotal = pt, agemin = minage, agemax = maxage, numfatal = cr[1,4],
                                 day = cr[1,5], month = cr[1,6], year = cr[1,7], date = cr[1,8], 
                                 modelyear = cr[1,9], make = cr[1,10], cellphoneuse = cr[1,11]))
    
  }
  return(ndf)
}

yesdf16na <- yesnadf(fatal_16nads)

yesdf <- function(df) {
  ydf <- df[df$duplicate == "yes",]
  drop <- c("duplicate","pernum")
  ydf = ydf[,!(names(ydf) %in% drop)]
  
  ydf$casestateveh <- paste(ydf$casenum,ydf$statenum,ydf$vehnum, sep = "")
  ndf <- data.frame(casenum = numeric(), statenum = numeric(), vehnum = numeric(), persontotal = numeric(),
                    agemin = numeric(), agemax = numeric(),
                    numfatal = numeric(), day = numeric(), month = numeric(),
                    year = numeric(), date = character(), modelyear = numeric(), make = numeric(), cellphoneuse = numeric())
  
  caselist <- unique(ydf$casestateveh)
  for (i in caselist) {
    cdf <- ydf[ydf$casestateveh == i,]
    
    pt <- length(cdf$casenum)
    
    cr <- cdf
    drop <- "age"
    cr = cr[,!(names(cr) %in% drop)]
    cr <- unique(cr)
    
    minage <- min(cdf$age)
    maxage <- max(cdf$age)
    ndf <- rbind(ndf, data.frame(casenum = cr[1,1], statenum = cr[1,2], vehnum = cr[1,3], 
                                 persontotal = pt, agemin = minage, agemax = maxage, numfatal = cr[1,4],
                                 day = cr[1,5], month = cr[1,6], year = cr[1,7], date = cr[1,8], 
                                 modelyear = cr[1,9], make = cr[1,10], cellphoneuse = cr[1,11]))
    
  }
  return(ndf)
  
}

yesdf16 <- yesdf(fatal_16ds)

nodf <- function(df) {
  ndf <- df[df$duplicate == "no",]
  drop <- c("duplicate", "pernum")
  ndf = ndf[,!(names(ndf) %in% drop)]
  ndf$casestateveh <- paste(ndf$casenum,ndf$statenum,ndf$vehnum, sep = "")
  mdf <- data.frame(casenum = numeric(), statenum = numeric(), vehnum = numeric(), persontotal = numeric(),
                    agemin = numeric(), agemax = numeric(),
                    numfatal = numeric(), day = numeric(), month = numeric(),
                    year = numeric(), date = character(), modelyear = numeric(), make = numeric(), cellphoneuse = numeric())
  caselist <- unique(ndf$casestateveh)
  for (i in caselist) {
    cdf <- ndf[ndf$casestateveh == i,]
    pt <- length(cdf$casenum)
    cr <- cdf
    #print(cr)
    minage <- cdf$age
    maxage <- cdf$age
    mdf <- rbind(mdf, data.frame(casenum = cr[1,1], statenum = cr[1,2], vehnum = cr[1,3], 
                                 persontotal = pt, agemin = minage, agemax = maxage, numfatal = cr[1,5],
                                 day = cr[1,6], month = cr[1,7], year = cr[1,8], date = cr[1,9], 
                                 modelyear = cr[1,10], make = cr[1,11], cellphoneuse = cr[1,12]))
    
  }
  return(mdf)
}

nodf16 <- nodf(fatal_16ds)
nodf16na <- nodf(fatal_16nads)

fulldataset <- rbind(yesdf16, nodf16)
fulldatasetna <- rbind(yesdf16na, nodf16na)


tableToCSV <- function(df) {
  #df = fulldata
  row.names(df)<- NULL
  colnames(df) <- c("casenum", "statenum", "vehnum", "persontotal","agemin","agemax","numfatal","day","month",
                    "year","date","modelyear","make","cellphoneuse")
  write.csv(df, file = "fulldatasetna.csv")
}
tableToCSV(fulldatasetna)
