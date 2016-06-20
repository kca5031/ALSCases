## R code adapted from Kyle Walker, PhD, Texas Christian University (https://github.com/walkerke/teaching-with-datavis/blob/master/pyramids/rcharts_pyramids.R)[2]

library(XML)

## Read in incidence and survival rate files on repository or see below

maleinc <- read.csv("MaleIncidence.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
femaleinc <- read.csv("FemaleIncidence.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

## Code:
  
ALSCases<-function(countries, year, gender){
  popdata<- function(country, year, gender) {
      c1 <- "http://www.census.gov/population/international/data/idb/region.php?N=%20Results	%20&T=10&A=separate&RT=0&Y="  
      c2 <- "&R=-1&C="
      yrs <- gsub(" ", "", toString(year))
      url <- paste0(c1, yrs, c2, country)
      df <- data.frame(readHTMLTable(url))
      nms <- c("Year", "Age", "Both Sexes Population", "Male", "Female", "percent", "pctMale", "pctFemale", 	"sexratio")  
      names(df) <- nms
      cols <- c(1, 3:9)
      df[,cols] <- apply(df[,cols], 2, function(x) as.numeric(as.character(gsub(",", "", 		x))))
      mnames <- c("male", "Male", "m", "M")
      fnames <- c("female", "Female", "f", "F")
      if(gender %in% mnames){
        keep <- c("Male")
        row.names(maleinc) <- c("Survival", "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", ">100")
        prev <- do.call(rbind.data.frame, apply(maleinc[2:dim(maleinc)[1], 2:length(maleinc)], 1, function(x) x*maleinc[1, 2:length(maleinc)]))
        write.csv(prev, file = "maleprev.csv")
        assign("prev", prev, envir = .GlobalEnv)
      }
      if(gender %in% fnames){
        keep <- c("Female")
        row.names(femaleinc) <- c("Survival", "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", ">100")
        prev <- do.call(rbind.data.frame, apply(femaleinc[2:dim(femaleinc)[1], 2:length(femaleinc)], 1, function(x) x*femaleinc[1, 2:length(femaleinc)]))
        write.csv(prev, file = "femaleprev.csv")
        assign("prev", prev, envir = .GlobalEnv)
      }
      df <- df[, keep, drop=FALSE]
      return(df)
  }
  data<-data.frame(sapply(countries, popdata, year, gender))
  data<-`names<-`(data, countries)
  drop.ref <- seq(1, dim(data)[1], 22)
  cut.popdata<-data[-(drop.ref), ]
  cases <- cut.popdata
  for (i in 1:length(countries)){
    country.code <- countries[i]
    cases[country.code] <- cut.popdata[, country.code] * prev[, country.code]/100000
  }
  cases$Year <- rep(year, each=21)
  cases$Ages <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
            "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", ">100")
  cases <- cases[, c("Year", "Ages", countries)]
  write.csv(cases, file= paste(ifelse(gender=="m"|gender=="M"|gender=="male"| gender=="Male", "Male", "Female"), "Cases.csv", sep=""), row.names = FALSE)
  return(cases)}

#Save all of the European Union 28 countries under one variable
EU28 <- c('AU','BE','BU','HR','CY','EZ','DA','EN','FI','FR','GM','GR','HU',	'EI',	'IT',	'LG',	'LH',	'LU',	'MT',	'NL',	'PL',	'PO',	'RO',	'LO',	'SI',	'SP',	'SW',	'UK')

#Run the R code for males as follows:
ALSCases(c('LY', 'US', 'UY', 'CH', 'IR', 'JA', 'TW', 'RI', 'NZ', EU28), c(2015, 2040), 'male')
#This will output "maleprev.csv" containing the age-specific male prevalence rates by country, and "MaleCases.csv" containing the number of male cases by country, year and age group to the working directory.

#Run the R code for females as follows:
ALSCases(c('LY', 'US', 'UY', 'CH', 'IR', 'JA', 'TW', 'RI', 'NZ', EU28), c(2015, 2040), 'female')
#This will output "femaleprev.csv" containing the age-specific female prevalence rates by country, and "FemaleCases.csv" containing the number of female cases by country, year and age group to the working directory.

