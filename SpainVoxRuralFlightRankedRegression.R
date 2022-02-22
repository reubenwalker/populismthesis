
#Upload municipality to data frame from SQL output
municipvox <- read.csv("C:\\Users\\reube\\OneDrive\\Documents\\R\\MunicFlightVoxUnemploymentEducation2.csv")

#Column names: municipality name, rural flight, vox vote share, population density, education, unemployment
colnames(municipvox) <- c("munname","ruralflight","voxperc","density","education","unemployment")

#Plot rural flight against vox vote share as percentages
a <- as.numeric(municipvox$ruralflight)*100
b <- as.numeric(municipvox$voxperc)*100
plot(a,b, main = "Vox Vote Share vs Spanish Age 18-49 Percentage Population Shift '15-'19",
     xlab = "Spanish Province Population Change '15-'19 as Percentage of 2015 Total Population",
     ylab = "Vox 2019 Vote Share (%)",
     pch = 19, frame = FALSE)
#Do they correlate?
test <- cor.test(a,b, use="pairwise.complete.obs")
test

#Looks pretty messy!

#Now we'll add the controls to see if it cleans up the data at all.
regressiondata <- municipvox[c("ruralflight",
					"voxperc",
					"education",
					"unemployment")]

#Add a binary dummy variable for positive flight
regressiondata$dummy <- ifelse(regressiondata$ruralflight > 0,1,0)

#Make sure our data is numeric
rData <- lapply(regressiondata,as.numeric)

#Pull all of the correlation coefficients for our correlation table
cor.test(rData$voxperc,rData$ruralflight)
cor.test(rData$voxperc,rData$unemployment)
cor.test(rData$voxperc,rData$education)
cor.test(rData$ruralflight,rData$unemployment)
cor.test(rData$ruralflight,rData$education)
cor.test(rData$education,rData$unemployment)

#Let's do some histograms!
#Vox vote share
hist(rData$ruralflight,main="Rural Flight Total Spain",xlab="Rural Flight '15-'19")
hist(b,main="Vox Share Total Spain",xlab="Vox Vote Share (%)")
hist(municipvox$ruralflight, main="Spain Rural Flight",
					xlab="Rural Flight '15-'19 percentage of 2015 population")

#These show that unemployment and education are essentially categorical variables by region.
hist(rData$unemployment)
#Employment looks a little more normal
hist(rData$education)

#Take subsets of municipalities with positive flight
flightpos <- subset(municipvox, ruralflight > 0, voxperc !=0)
flightneg <- subset(municipvox, ruralflight < 0, voxperc !=0)

flightposNOZERO <- subset(municipvox, ruralflight > 0, voxperc !=0)
flightnegNOZERO <- subset(municipvox, ruralflight < 0, voxperc !=0)

voxpos <- flightpos$voxperc*100
hist(voxpos,xlim=c(0,70),ylim=c(0,500),main="Vox Percent Positive Rural Flight",xlab="Vox Vote Share (%)")
voxneg <- flightneg$voxperc*100
hist(voxneg,xlim=c(0,70),ylim=c(0,500),main="Vox Percent Negative Rural Flight",xlab="Vox Vote Share (%)")

plot(flightpos$ruralflight,voxpos, 
	main = "Vox Vote Share vs Spanish Age 18-49 Percentage (Positive) Rural Flight'15-'19",
     xlab = "Spanish Province Population Change '15-'19 as Percentage of 2015 Total Population",
     ylab = "Vox 2019 Vote Share (%)",
     pch = 19, frame = FALSE)

plot(flightneg$ruralflight,voxneg, 
	main = "Vox Vote Share vs Spanish Age 18-49 Percentage (Negative) Rural Flight'15-'19",
     xlab = "Spanish Province Population Change '15-'19 as Percentage of 2015 Total Population",
     ylab = "Vox 2019 Vote Share (%)",
     pch = 19, frame = FALSE)

#Ok, we're getting errors for the poisson regression, because the values in the
#table are non-integers. I'm going to try to do a histogram of the data log-transformed.
hist(log(flightposNOZERO$voxperc),
    #xlim=c(0,70),
    #ylim=c(0,400),
    main="Log Transformed Vox Percent Positive Rural Flight",
    xlab="Log Transformed Vox Vote Share (%)")

hist(log(flightnegNOZERO$voxperc),
    #xlim=c(0,70),
    #ylim=c(0,400),
    main="Log Transformed Vox Percent Negative Rural Flight",
    xlab="Log Transformed Vox Vote Share (%)")

hist(log(flightposNOZERO$ruralflight),
    #xlim=c(0,70),
    #ylim=c(0,400),
    main="Log Transformed Positive Rural Flight",
    xlab="Log Transformed Rural Flight (%)")

hist(log(flightnegNOZERO$ruralflight*-1),
    #xlim=c(0,70),
    #ylim=c(0,400),
    main="Log Transformed Negative Rural Flight",
    xlab="Log Transformed Rural Flight (%)")


#Regression time
#

#Some simple regression attempts:
uncontrolled = lm(voxperc~ruralflight,
data = rData)
summary(uncontrolled)

#MODEL 1
#Dependent:Vox Vote Share
#Independent: Rural Flight, Unemployment, Education
controlled = lm(voxperc~ruralflight
				+ unemployment
				+ education,
data = rData)
summary(controlled)

#MODEL 2
#Dependent:Vox Vote Share
#Independent: Binary Flight, Unemployment, Education
controlledDummy = lm(voxperc~dummy
				+ unemployment
				+ education,
data = rData)
summary(controlledDummy)

#MODEL 3
#Dependent:Vox Vote Share
#Independent: Rural Flight, Binary Flight, Unemployment, Education
controlledDoubleFlight = lm(voxperc~ruralflight
				+ unemployment
				+ education
				+ dummy,
data = rData)
summary(controlledDoubleFlight)

rfArbeit = rfit.default(voxperc~ruralflight
						+ unemployment
						+education
						+dummy,
						
data = rData)
summary(rfArbeit)


#Further attempts to incorporate correlated independent variables
#MODEL 4
rData$unemployedPlusEducation <- (rData$unemployment + rData$education)
rData
singleControlFlight = lm(voxperc~ruralflight
				+ unemployedPlusEducation,
data = rData)
summary(singleControlFlight )

#MODEL 5
rData$unemployedPlusEducation <- (rData$unemployment + rData$education)
rData
singleControlFlight = lm(voxperc~dummy
				+ unemployedPlusEducation,
data = rData)
summary(singleControlFlight)

cor(rData$ruralflight,rData$dummy)

rData$flightPlusBinary <- (rData$ruralflight + rData$dummy)
#MODEL 6
singlePredictor = lm(voxperc~flightPlusBinary
				+ unemployment
				+ education,
data = rData)
summary(singlePredictor)

