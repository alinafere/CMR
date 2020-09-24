# -------------------------------------------------------------------------
# R Script:  Slides for Consumer Marketing Research
#            R Session 1
# Author:    Pieter Schoonees (with contributions from Andreas Alfons)
# -------------------------------------------------------------------------

## Load a package
library("ggplot2")

## Help facilities
?help
help("help")
help(package = "ggplot2")
example("mean")

## Loading data
load("Prestige.RData")

## Inspecting data
head(Prestige)
tail(Prestige)
summary(Prestige)
str(Prestige)
dim(Prestige)
nrow(Prestige)
ncol(Prestige)
colnames(Prestige)

## Extracting variables and doing basic math
Prestige$education
Prestige$type
Prestige$women / 100
2 ^ Prestige$logincome

## Creating new variables
Prestige$income <- 2 ^ Prestige$logincome
head(Prestige)

## Simple statistics functions
min(Prestige$women)
max(Prestige$women)
range(Prestige$women)
mean(Prestige$logincome)
median(Prestige$logincome)
sd(Prestige$logincome)
table(Prestige$type)

## Bivariate statistics functions
cor(Prestige$women, Prestige$prestige)
table(Prestige$type, useNA = "always")

## Base graphics scatterplot matrix
plot(Prestige)

## Plotting with ggplot2

# library("ggplot2")

## Scatterplot
ggplot(Prestige, aes(x = prestige, y = logincome)) 
ggplot(Prestige, aes(x = prestige, y = logincome)) + geom_point()

## Histogram
ggplot(Prestige, aes(x = prestige)) + geom_histogram()
ggplot(Prestige, aes(x = prestige)) + geom_histogram(bins = 15)

## Density plot
ggplot(Prestige, aes(x = prestige)) + geom_density()

## QQ plot
ggplot(Prestige, aes(sample = prestige)) + geom_qq()

## Boxplots
ggplot(Prestige, aes(x = "", y = prestige)) + geom_boxplot()
ggplot(Prestige, aes(x = type, y = prestige)) + geom_boxplot()

## Barplot
ggplot(Prestige, aes(x = type)) + geom_bar()

## Time series plot
load("Feyenoord.RData")
ggplot(Feyenoord, aes(x = Year, y = Points)) + geom_line()

## Loading SPSS data
library("haven")
session2 <- read_sav("Session 2 example.sav")
setwd("~/Dropbox/erasmus teaching/Alina_Ferecatu_CMR/CMR 2018/assignments")
session2 <- read_sav("CMR2018_Assignments_Data_Going_Green_Samples.sav")
head(session2)
with(session2, table(Income_parents))
## Exercises 1: Solutions
load("patents.RData")

## Q1
min(patents$total)
max(patents$total)
range(patents$total)

## Q2
cor(patents$logtotal, patents$logdensity)

## Q3
table(patents$governor)

## Q4
table(patents$governor, patents$areacat)

## Q5
patents$proputility <- patents$utility / patents$total
mean(patents$proputility)
median(patents$proputility)
sd(patents$proputility)

## Exercises 2: Solutions

## Q1
ggplot(patents, aes(x = density, y = total)) + geom_point()
ggplot(patents, aes(x = logdensity, y = logtotal)) + geom_point()

## Q2
ggplot(patents, aes(x = total)) + geom_histogram(bins = 15)
ggplot(patents, aes(x = logtotal)) + geom_histogram(bins = 15)

## Q3
ggplot(patents, aes(x = "", y = logdensity)) + geom_boxplot()

## Q4
ggplot(patents, aes(x = densitycat, y = total)) + geom_boxplot()
ggplot(patents, aes(x = densitycat, y = logtotal)) + geom_boxplot()

## Q5
ggplot(patents, aes(x = governor)) + geom_bar()
ggplot(patents, aes(x = areacat)) + geom_bar()

