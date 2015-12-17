# Quick Bedford Law thing
library(ggplot2)
library(plyr)
require(scales)

# Load Data
SenateReturn <- read.csv("H:/Senate2014.csv", stringsAsFactors=FALSE)
SenateReturn <- read.csv("C:/Users/Olson/Desktop/Benford/Senate2014.csv", stringsAsFactors=FALSE)


## Drop Precints where leading number is zero
SenateReturn <- subset(SenateReturn, SenRFirst > 0)

# Create Benford Distro
(benford=log(1+1/(1:9))/log(10))
names(benford)=1:9

# Plot Senate Data
SenatePlot <- ggplot(SenateReturn, aes(x=SenRFirst))
SenatePlot <- SenatePlot + theme_bw()
SenatePlot <- SenatePlot + geom_histogram(aes(y = ..density..), binwidth = 1, breaks=seq(1, 10, by=1))
SenatePlot <- SenatePlot + labs(title = "Distribution of All First Digits, Republican Candidate for Senate 2014", x = "First Digits in Precinct")
SenatePlot <- SenatePlot + xlim(c(1,9)) + ylim(c(0,.3)) + scale_x_continuous(breaks=pretty_breaks(n=9))

#Plot Benford Data
barplot(benford,col="white",ylim=c(-.045,.3))
abline(h=0)
title(main="Distribution of First Digits according to Benford's Law", 
      xlab="Digit", ylab="Proportion of Digits")

# Try the analysis with a package
## Load Package
install.packages("benford.analysis")
library(benford.analysis)

## Run Analysis
bfd.cp <- benford(SenateReturn$SenR)

## Look at it
plot(bfd.cp)
bfd.cp