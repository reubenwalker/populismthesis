#This is a comment. The code won't execute.

#Next we need to download our data. 
#We'll use a 'CSV' file (comma-separated values), basically one tab of a spreadsheet
#Gesamtfortzuege:
#Wahlkreiscode, Wahlkreisname, AfDVoteShare2017, AvgNetFortzuege2014-2017, AvgNetFortzuege%
#Fortzuege 18-50:
mediumF <- read.csv("C:\\Users\\reube\\OneDrive\\Documents\\R\\AfdShareNetFortzuege18-50.csv")

#Now we want to plot and look at our data:
#x axis is the independent variable, y axis is the dependent variable

x <- mediumF$FortzuegeProzentFrom2013Population
y <- mediumF$AfDShare2017

plot(x,y, main = "AfD Vote Share vs Age 18-50 Percentage Population Shift '14-'17",
     xlab = "Avg Yearly Emigration '14-'17 as Percentage of 2013 Total Population",
     ylab = "AfD 2017 Vote Share (%)",
     pch = 19, frame = FALSE)

#test <- cor.test(youngF$FortzuegeProzentFrom2013Population, youngF$AfDShare2017, use="pairwise.complete.obs")
test <- cor.test(x, y, use="pairwise.complete.obs")

#With this file, we have wahlkreiscode, wahlkreisname, AfDShare2017, Fortzuege18-50, Arbeitslosigkeit, EastWest, and Ausbildung
ausbildung <- read.csv("C:\\Users\\reube\\OneDrive\\Documents\\R\\AusbildungQuery.csv")

regressiondata <- ausbildung[c("AfDShare2017",
					"FortzuegeProzentFrom2013Population",
					"Arbeitslos",
					"EastWest",
					"Ausbildung")]
rData <- lapply(regressiondata,as.numeric)
regressiondataEast <- subset(regressiondata, EastWest == 1)
rDataEast <- lapply(regressiondataEast,as.numeric)

#Regression uncontrolled
lmArbeit = lm(AfDShare2017~FortzuegeProzentFrom2013Population,
data = rData)
summary(lmArbeit)

#Regression controlling unemployment as percentage of population 
lmArbeit = lm(AfDShare2017~FortzuegeProzentFrom2013Population 
						+ Arbeitslos
						+Ausbildung,
data = rData)
summary(lmArbeit)

#Regression for East, controlling unemployment and ausbildung
#as percentage of population 
lmArbeit = lm(AfDShare2017~FortzuegeProzentFrom2013Population 
						+ Arbeitslos
						+Ausbildung,
data = rDataEast)
summary(lmArbeit)
