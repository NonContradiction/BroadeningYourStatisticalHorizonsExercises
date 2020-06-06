#Assignments in this doc: 
#Create Figure 1.4.
#Fit Model 4 (p. 19), obtain CI and prediction.

#Problem 1: 
# Create figure 1.4

#Imports/libraries
library(ggplot2)

#We pull the data from the csv at the Github URL 
mydata <- read.csv("https://raw.githubusercontent.com/proback/BYSH/master/data/derbyplus.csv")

#And glance at it: 
#View(mydata)

#We derive the column fastfactor: 
mydata$fastfactor <- mydata$condition
#It's a factor so we bother with this: 
levels(mydata$fastfactor) <- c(levels(mydata$fastfactor), 'not fast')
mydata$fastfactor[mydata$condition!='fast'] <- 'not fast'

#Again, can visually inspect: 
#View(mydata)

#And drop extraneous levels: 
mydata$fastfactor <- factor(mydata$fastfactor)

#Double check it: 
#levels(mydata$fastfactor) 

#And we do the plotting
#scatter plot
ggplot(mydata, aes(x=year, y=speed, color=fastfactor))  + 
  geom_point()+
  #With regression lines: 
  geom_smooth(method=lm, se=FALSE)

#Fit Model 4
#We derive the other variables that we have to: 

#yearnew
mydata$yearnew <- mydata$year - 1896

#Visual inspection: 
#View(mydata)

#fast
mydata$fast <- rep(0, nrow(mydata))
#It's a factor so we bother with this: 
mydata$fast[mydata$fastfactor=='fast'] <- 1

#Visual inspection: 
#mydata$fast

#We fit the model: 
model4 <- lm(formula = speed ~ yearnew + fast, data=mydata)

#We print out the summary: 
summary(model4)

#Here is the required confidence interval: 
confint(model4)

#And we generate the required prediction: 
new.data <- data.frame(yearnew = 2017 - 1896, fast=1)
predict(model4, new=new.data, interval = "prediction")