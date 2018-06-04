# Modify the path below to match your folders structure
projFld <- "/Users/namsan/Desktop/Spring\ 2018/Econometrics/Project"

# Shortcuts to folders of interest
CleanData <- paste0(projFld,"/CleanData")
Dictionaries <- paste0(projFld,"/Dictionaries")
RawData <- paste0(projFld,"/RawData")
RCode <- paste0(projFld,"/RCode")
RData <- paste0(projFld,"/RData")
Output <- paste0(projFld,"/Output")

# use data.table 
#install.packages("data.table")
library(data.table)
# use readxl package
#install.packages("readxl")
library(readxl)
# use reshape package
#install.packages("reshape")
library(reshape)

# point to the files Patent resident
rawpatentres <- paste0(RawData,"/patentres.xls")
raw.patent.res <- data.table(read_excel(rawpatentres, sheet = 1))

# point to the files Patent non resident 
rawpatentnon <- paste0(RawData,"/patentnon.xls")
raw.patent.non <- data.table(read_excel(rawpatentnon, sheet = 1))

# point to the files GDP 
rawGDP <- paste0(RawData,"/GDPpercapita.xls")
raw.GDP <- data.table(read_excel(rawGDP, sheet = 1))

# function to melt data 
handleData <- function(data.in, value = "value", variable.name = "year") {
  datain <- copy(data.in)
  #change variable name
  old.names <- colnames(datain)
  new.names <- tolower(old.names)
  new.names <- gsub(" ", ".", new.names)
  colnames(datain) <- new.names
  #meltdata
  melt1 <- melt(datain, id.vars = c("country.code", "country.name"), value.name = value)
  #arrange in order
  melt.order <- melt1[order(melt1$country.code),]
  setnames(melt.order, "variable", variable.name)
  melt.order
}

patent.res <- handleData(raw.patent.res, value = "patent.res")
patent.non <- handleData(raw.patent.non, value = "patent.non")
data.GDP <- handleData(raw.GDP, value = "GDP")
data.test <- merge(patent.res, data.GDP, by=c("country.code","country.name","year"))
data.test1 <- merge(data.test, patent.non, by=c("country.code","country.name","year"))

#Descriptive analysis 

str(data.test1)
summary(data.test1)
write.csv(data.test1, file = "econ.csv")
