#Assignments in this doc: 
#Chapter 8

"""
Replicate analysis in: §8.5.5
Replicate analysis in: §8.8.0, §8.10
(in 8.10, only include random effects for intercept, previous, and Public)
"""

#Chapter 8: Multi-level models
#Analyses of Random and Fixed Effects in Models of Groups of Data

###Replicate analysis in: §8.5.5

#################
#Prep Data / Early Chapter Work
#################

#We read in the data: 
mydata <- read.csv("https://raw.githubusercontent.com/proback/BYSH/master/data/musicdata.csv")

#Take a look at it
#View(mydata)
names(mydata)

#Exploratory (graphical) analysis
#We plot NAs by frequency, raw
hist(mydata$na, xlab='Negative Affect', main='(a)')

#We compress NAs into means by performer (ID)
usedf <- mydata[,c(2, 9)]
#View(usedf)
second <- aggregate(usedf[, 2], list(usedf$id), mean)
#View(second)

library(ggplot2)

theme_set(
  theme_dark()
)

ggplot(second, aes(x=x)) + 
  geom_histogram(binwidth=2, color="black", fill="white") +
  scale_x_continuous(limits = c(9, 36))

#And plot the other things mentioned in the book: 
plot(mydata$perform_type)
plot(mydata$audience)
plot(mydata$memory)

##########################
#Model in 8.5.5
##########################

#We grab just the columns we want to work with
select<-c("id", "perform_type", "instrument", "na")
selectedcols <- mydata[,select]
#View(selectedcols)

#And recode the factors as numeric binary 1/0
levels(selectedcols$perform_type)
selectedcols$large <- factor(selectedcols$perform_type,
                   levels = levels(selectedcols$perform_type),
                   labels = c(1, 0, 0))
selectedcols$large <- as.numeric(as.character(selectedcols$large))

levels(selectedcols$instrument)
selectedcols$orch <- factor(selectedcols$instrument,
                                   levels = levels(selectedcols$instrument),
                                   labels = c(0, 1, 0))
selectedcols$orch <- as.numeric(as.character(selectedcols$orch))

#And we look at it to make sure that worked: 
#View(selectedcols)

#The book uses the lmer function so we do too: 
#install.packages("lme4")
library(lme4)

#We buld the model with two factors, an interaction term, and a Level Two grouping term
multilevels <- lmer(formula = na ~ orch + large + orch:large + (large|id), data=selectedcols)
summary(multilevels)

#######PART TWO
#Chapter 8: Multi-level models (part two)

###Use R and SAS to replicate analysis in: §8.8.0, §8.10 (in 8.10, only include random effects for intercept, previous, and Public)

#################
#Prep Data / Early Chapter Work
#################

#We read in the data again if necessary: 
#mydata <- read.csv("https://raw.githubusercontent.com/proback/BYSH/master/data/musicdata.csv")

#Take a look at it
#View(mydata)
names(mydata)

##########################
#Model in 8.8.0
##########################

#We grab just the columns we want to work with
select<-c("id", "perform_type", "instrument", "mpqnem", "na")
selectedcols <- mydata[,select]
#View(selectedcols)

#And recode the factors
#Perform type is large = 1 else = 0
levels(selectedcols$perform_type)

selectedcols$large <- factor(selectedcols$perform_type,
                             levels = levels(selectedcols$perform_type),
                             labels = c(1, 0, 0))
selectedcols$large <- as.numeric(as.character(selectedcols$large))
#instrument is orch = 1 else = 0
levels(selectedcols$instrument)

selectedcols$orch <- factor(selectedcols$instrument,
                            levels = levels(selectedcols$instrument),
                            labels = c(0, 1, 0))
selectedcols$orch <- as.numeric(as.character(selectedcols$orch))

#And we look at it to make sure that worked: 
#View(selectedcols)

#The book uses the lmer function so we do too: 
#install.packages("lme4")
library(lme4)

#We buld the model with two factors, an interaction term, and a Level Two grouping term
multilevels <- lmer(formula = na ~ orch mpqnem large orch:large mpqnem:large (large|id), data=selectedcols)
summary(multilevels)

##########################
#Model in 8.10 (in 8.10, only include random effects for intercept, previous, and Public)
##########################

#Much feature engineering, as per the instructions of the book

#We grab just the columns we want to work with
select<-c("id", "diary", "audience", "perform_type", "mpqpem", "mpqnem", "mpqab", "instrument", "na")
selectedcols <- mydata[,select]
#View(selectedcols)
#And recode the factors

#Previous is diary - 1
selectedcols$previous <- selectedcols$diary - 1

#students is audience: students = 1 else = 0
levels(selectedcols$audience)

selectedcols$students <- factor(selectedcols$audience,
                                levels = levels(selectedcols$audience),
                                labels = c(0, 0, 0, 1))
selectedcols$students <- as.numeric(as.character(selectedcols$students))

#juried is audience: juried = 1 else = 0
#levels(selectedcols$audience)
selectedcols$juried <- factor(selectedcols$audience,
                              levels = levels(selectedcols$audience),
                              labels = c(0, 1, 0, 0))
selectedcols$juried <- as.numeric(as.character(selectedcols$juried))

#public is audience: public = 1 else = 0
#levels(selectedcols$audience)
selectedcols$public <- factor(selectedcols$audience,
                              levels = levels(selectedcols$audience),
                              labels = c(0, 0, 1, 0))
selectedcols$public <- as.numeric(as.character(selectedcols$public))

#solo is perform_type: solo = 1 else = 0
levels(selectedcols$perform_type)

selectedcols$solo <- factor(selectedcols$perform_type,
                            levels = levels(selectedcols$perform_type),
                            labels = c(0, 0, 1))
selectedcols$solo <- as.numeric(as.character(selectedcols$solo))

#instrument is orch = 1 else = 0
levels(selectedcols$instrument)

selectedcols$orch <- factor(selectedcols$instrument,
                            levels = levels(selectedcols$instrument),
                            labels = c(0, 1, 0))
selectedcols$orch <- as.numeric(as.character(selectedcols$orch))

#And we look at it to make sure that worked: 
#View(selectedcols)

#We buld our nested model of the book's "final" multilevel model
multilevels <- lmer(formula = na ~ previous + students + juried + public + solo + mpqpem + 
                    mpqab + orch + mpqnem + mpqnem:solo (previous + public | id), data=selectedcols)
summary(multilevels)
#We note that though the model failed to converge, its estimates are almost exactly those of our SAS code.
