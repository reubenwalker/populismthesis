#First, we need to download our data. 
#We'll use a 'CSV' file (comma-separated values), basically one tab of a spreadsheet

#With this file, we have wahlkreiscode, wahlkreisname, AfDShare2017, Fortzuege18-50, Arbeitslosigkeit, EastWest, and Ausbildung
ausbildung <- read.csv("C:\\Users\\reube\\OneDrive\\Documents\\R\\ArbeitslosAusbildung18-50_20211129 (2).csv")

colnames(ausbildung) <- c("Wahlkreiscode","Wahlkreisname","AfDShare2017",
					"FortzuegeProzentFrom2013Population",
					"Arbeitslos","EastWest","Ausbildung")

regressiondata <- ausbildung[c("AfDShare2017",
					"FortzuegeProzentFrom2013Population",
					"Arbeitslos",
					"EastWest",
					"Ausbildung")]

#We create a dummy variable for Flight/No Flight
regressiondata$BinaryFlight <- ifelse(regressiondata$FortzuegeProzentFrom2013Population> 0,1,0)

#rDataCont <- rData[c("AfDShare2017",
#					"FortzuegeProzentFrom2013Population",
#					"Arbeitslos",
#					"Ausbildung")]
#

#Here we make sure all of our data is numeric
rData <- lapply(regressiondata,as.numeric)

#Here we take a look at the correlations for our different metrics.
res <- cor(rData$AfDShare2017,rData$FortzuegeProzentFrom2013Population)
res <- cor(rData$AfDShare2017,rData$Arbeitslos)
res <- cor(rData$AfDShare2017,rData$Ausbildung)
res <- cor(rData$FortzuegeProzentFrom2013Population,rData$Arbeitslos)
res <- cor(rData$FortzuegeProzentFrom2013Population,rData$Ausbildung)
res <- cor(rData$Arbeitslos,rData$Ausbildung)

#This is just using T-Tests to get to the correlation faster.
cor.test(rData$AfDShare2017,rData$FortzuegeProzentFrom2013Population)
cor.test(rData$AfDShare2017,rData$Arbeitslos)
cor.test(rData$AfDShare2017,rData$Ausbildung)
cor.test(rData$FortzuegeProzentFrom2013Population,rData$Arbeitslos)
cor.test(rData$FortzuegeProzentFrom2013Population,rData$Ausbildung)
cor.test(rData$Arbeitslos,rData$Ausbildung)
round(res,2)

#Taking a look at some histograms
hist(rData$Arbeitslos)
hist(rData$Ausbildung)

#Normality Tests
library("dplyr")
library("ggpubr")

#Checking our data for normality
shapiro.test(rData$AfDShare2017)
shapiro.test(rData$FortzuegeProzentFrom2013Population)
shapiro.test(rData$Arbeitslos)
shapiro.test(rData$Ausbildung)


#Here we split the data into east and west subsets for an eventual regression
regressiondataEast <- subset(regressiondata, EastWest == 1)
regressiondataWest <- subset(regressiondata, EastWest == 0)
rDataEast <- lapply(regressiondataEast,as.numeric)
rDataWest <- lapply(regressiondataWest,as.numeric)

#Regression uncontrolled
lmArbeit = lm(AfDShare2017~FortzuegeProzentFrom2013Population,
data = rData)
summary(lmArbeit)

#MODEL 1
#Regression controlling unemployment and education as percentage of population 
#Dependent: AfD Vote Share
#Independent: Rural Flight, Unemployment, Education
lmArbeit = lm(AfDShare2017~FortzuegeProzentFrom2013Population 
						+ Arbeitslos
						+Ausbildung,
data = rData)
summary(lmArbeit)

#MODEL 2
#Regression controlling unemployment, education, and RuralFlightDummy as percentage of population 
#Dependent: AfD Vote Share
#Independent: Rural Flight, Unemployment, Education, EastWest
lmArbeit = lm(AfDShare2017~FortzuegeProzentFrom2013Population 
						+ Arbeitslos
						+Ausbildung
						+EastWest,
data = rData)
summary(lmArbeit)

#MODEL 3
#Regression without FortzuegeProzent
#Dependent: AfD Vote Share
#Independent: Binary, Rural Flight, Unemployment, Education, EastWest
lmArbeit = lm(AfDShare2017~BinaryFlight
						+ Arbeitslos
						+Ausbildung
						+EastWest,
						
data = rData)
summary(lmArbeit)

rfArbeit = rfit.default(AfDShare2017~BinaryFlight
						+ Arbeitslos
						+Ausbildung
						+EastWest,
						
data = rData)
summary(rfArbeit)

#MODEL 4
#Regression for East, controlling unemployment and ausbildung
#as percentage of population 
#Dependent: AfD Vote Share
#Independent: Rural Flight, Unemployment, Education
lmArbeit = lm(AfDShare2017~FortzuegeProzentFrom2013Population 
						+ Arbeitslos
						+Ausbildung,
data = rDataEast)
summary(lmArbeit)

#MODEL 5
#Regression for West, controlling unemployment and ausbildung
#as percentage of population 
#Dependent: AfD Vote Share
#Independent: Rural Flight, Unemployment, Education
lmArbeit = lm(AfDShare2017~FortzuegeProzentFrom2013Population 
						+ Arbeitslos
						+Ausbildung,
data = rDataWest)
summary(lmArbeit)

