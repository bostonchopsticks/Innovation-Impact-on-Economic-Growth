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
library(ggplot2)
#install.packages("GGally")
library(GGally)

# point to the files
rawData <- paste0(RawData,"/econ.csv")
rawData <- data.table(read.csv(rawData, sep = ',', stringsAsFactors = F))
length(unique(rawData$country.name))
rawData$X <- NULL

# merge fixbroadband variable to the data set 
fixbroadband <- paste0(RawData,"/fixbroadband.csv")
fixbroadband <- data.table(read.csv(fixbroadband, sep = ',', stringsAsFactors = F))
old.names <- colnames(fixbroadband)
new.names <- gsub("X", "", old.names)
colnames(fixbroadband) <- new.names

category <- paste0(RawData,"/category.xls")
category.country <- data.table(read_excel(category, sheet = 2))

fixbroadband <- handleData(fixbroadband, value = "fixed.broadband")
fixbroadband$year <- as.integer(as.character(fixbroadband$year))

data.test <- merge(rawData, fixbroadband, by=c("country.code","country.name","year"))
data.final <- merge(data.test, category.country, by= "country.code")

#write.csv(data.final, file = "econdatafinal.csv")

data.final$income.group <- as.factor(data.final$income.group)

# descriptive analysis 
str(data.final)
summary(data.final)
#sapply(data.final, levels)

#correlation plot 
ggcorr(data.final[, 4:7], label = TRUE, hjust=0.62, size = 5)

# take log of the variable
data.final1 <- copy(data.final)
data.final1$patent.res <- log(data.final1$patent.res)
data.final1$GDP <- log(data.final1$GDP)
data.final1$patent.non <- log(data.final1$patent.non)
data.final1$fixed.broadband <- log(data.final1$fixed.broadband)

# now the correlation changes
ggcorr(data.final1[, 9:12], label = TRUE, hjust=0.56, size = 4)

ggplot(data.final1, aes(x = patent.res , y = GDP, colour = income.group, shape=income.group)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)


library(car)
scatterplot(GDP ~ year|income.group, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=data.final1)

## Fixed-Effects model 

#ggplot(data.final, aes(x = year , y = GDP, colour = income.group)) +
#  geom_point()
library(gplots)
#plotmeans(GDP ~ income.group, main="Heterogeineityacross countries", data=data.final)
plotmeans(GDP ~ year, main="Heterogeineity across years", barcol="red", data=data.final1)

## OLS 

OLS <- lm(formula = GDP  ~ patent.res + patent.non + fixed.broadband, data = data.final)
summary(OLS)

OLS1 <- lm(formula = GDP  ~ patent.res + patent.non + fixed.broadband, data = data.final1)
summary(OLS1)

library(alr4)
residualPlots(OLS)
residualPlots(OLS1)
par(mfrow=c(2,2))
par(mfrow=c(1,1))
plot(OLS1)

##re run OLS 
# OLS 

OLS <- lm(formula = GDP  ~ patent.res + patent.non + fixed.broadband, data = data.final)
summary(OLS)
#residualPlots(OLS)
par(mfrow=c(2,2))

plot(OLS)

par(mfrow=c(1,1))
plot(data.final1$patent.res, data.final1$GDP, pch=19, xlab="x1", ylab="y")
abline(lm(data.final1$GDP~data.final1$patent.res),lwd=3, col="red")

## Fixed effects using Least squares dummy variable model

least.squares.dum <-lm(GDP  ~ patent.res + patent.non + fixed.broadband 
               + factor(country.name) -1, data=data.final1)
summary(least.squares.dum)

install.packages('apsrtable')
library(apsrtable)
apsrtable(OLS,least.squares.dum, model.names= c("OLS", "OLS_DUM"))

## Fixed effects: Identity-specific intercepts
#install.packages("plm")
library(plm)
fixed <-plm(GDP  ~ patent.res + patent.non + fixed.broadband, data=data.final1, index=c("country.name", "year"), model="within")
summary(fixed)

fixed.time<-plm(GDP  ~ patent.res + patent.non + fixed.broadband + factor(year), data=data.final1, index=c("country.name", "year"), model="within")
summary(fixed.time)
pFtest(fixed.time, fixed)
plmtest(fixed, c("time"), type=("bp"))

# Display the fixed effects (constants for each country)
fixef(fixed)

# Testing for fixed effects, null: OLS better than fixed
pFtest(fixed, OLS)


## RANDOM EFFECTS 

random <-plm(GDP  ~ patent.res + patent.non + fixed.broadband
             , data=data.final1, index=c("country.name", "year"), model="random")
summary(random)
phtest(fixed, random)


library(corrplot)
corrplot(cor(data.final), method='number')
# select only numberic columns
numcols <- sapply(data.final, is.numeric)
data.plot <- data.final[,(numcols),with=F]
data.plot$year <- NULL
corrplot(cor(na.omit(data.plot)))
corrplot(cor(na.omit(data.plot), method = 'pie')


#test stationary yes 
Panel.set<-plm.data(data.final1, index = c("country.name", "year"))
install.packages("tseries")
library(tseries)
adf.test(Panel.set$GDP, k=2)

#test serial correlation
pbgtest(fixed)

#test homoskedasticity
library(lmtest)
bptest(GDP  ~ patent.res + patent.non + fixed.broadband + factor(country.name), data = data.final1, studentize=F)

# fix serial correlation and homoskedasticity in fixed effects model 
coeftest(fixed)
coeftest(fixed, vcovHC)
# Heteroskedasticityconsistent coefficients (Arellano)
coeftest(fixed, vcovHC(fixed, method = "arellano"))
# Heteroskedasticityconsistent coefficients, type 3
coeftest(fixed, vcovHC(fixed, type = "HC3"))

# The following shows the HC standard errors of the coefficients
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(fixed, type = x)))))

