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
install.packages("data.table")
library(data.table)
# use readxl package
install.packages("readxl")
library(readxl)
# use reshape package
install.packages("reshape")
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
#write.csv(data.test1, file = "econ.csv")


datalog <- copy(data.test1)
datalog$patent.res <- log(datalog$patent.res)
datalog$GDP <- log(datalog$GDP)
datalog$patent.non <- log(datalog$patent.non)
coplot(GDP ~ year|country.name, type="l", data=datalog) # Lines
coplot(GDP ~ year|country.name, type="b", data=datalog) # Points and lines
par("mar")
par(mar=c(5.1, 4.1, 4.1, 2.1))

library(car)
scatterplot(GDP ~ year|country.name, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=data.test1)

library(gplots)
plotmeans(GDP ~ year, main="Heterogeineityacross years", data.test1)
plotmeans(GDP ~ country.name, main="Heterogeineityacross countries", data=data.test1)

OLSpanel <- lm(formula = GDP  ~ patent.res, data = datalog)
summary(OLSpanel)

library(alr4)
residualPlots(OLSpanel)
par(mfrow=c(1,1))
plot(OLSpanel)


OLSfixed <- lm(formula = GDP  ~ patent.res + factor(country.name) -1, data = datalog)
summary(OLSfixed)


yhat<-OLSfixed$fitted


scatterplot(yhat~datalog$patent.res|datalog$country.name, boxplots=FALSE, xlab="patent.res", ylab="yhat",smooth=FALSE)
abline(lm(data.test1$GDP~data.test1$patent.res),lwd=3, col="red")




install.packages("plm")
library(plm)
fixed <-plm(GDP  ~ patent.res + patent.non, data=datalog, index=c("country.name", "year"), model="within")
summary(fixed)

fixef(fixed)
pFtest(fixed, OLSpanel)

# random effects
random <-plm(GDP  ~ patent.res + patent.non, data=datalog, index=c("country.name", "year"), model="random")
summary(random)

phtest(fixed, random)

library(foreign)
Panel <-read.dta("http://dss.princeton.edu/training/Panel101.dta")
require(ggplot2)
