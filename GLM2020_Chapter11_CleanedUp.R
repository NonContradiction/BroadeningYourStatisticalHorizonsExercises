#Chapter 11: GL Multilevel Models
###Replicate analyses in:
#§11.4.1, 2 in §11.4.3

#################
#Prep Data / Early Chapter Work
#################

#loading packages: 
#The book uses the lmer function so we do too: 
library(lme4)
library(ggplot2)
library(dplyr)

#We read in the data: 
mydata <- read.csv("https://raw.githubusercontent.com/proback/BYSH/master/data/basketball0910.csv")

#Take a look at it
#View(mydata)
names(mydata)

#Exploratory plots: 

p <- ggplot(mydata, aes(x=time)) + 
  geom_histogram(color="black", fill="white") 
p + ylab('frequency') + xlab('Time left in first half (minutes)')

q <- ggplot(mydata, aes(x=foul.diff)) + 
  geom_histogram(binwidth = 1.5, color="black", fill="white") 
q + ylab('frequency') + xlab('Foul difference (home-visitor')

r <- ggplot(mydata, aes(x=score.diff)) + 
  geom_histogram(color="black", fill="white") 
r + ylab('frequency') + xlab('Score difference (home-visitor)')

#analysis in section 11.4.1: 

glmmodel <- glm(formula = foul.home ~ foul.diff + score.diff + lead.home + time + 
      foul.diff:time + lead.home:time , family = binomial, 
    data = mydata)
summary(glmmodel)

#11.4.2: 2-stage modeling
game110 <- mydata[which(mydata$game == 110), ]

model110 <- glm(formula = foul.home ~ foul.diff, family = binomial, 
                data = game110)
summary(model110)

#Analysis in 11.4.3 (the "real" framework): 
multimodel <- glmer(formula = foul.home ~ foul.diff + (foul.diff | game), 
               data=mydata, family = binomial)
summary(multimodel)

bonusmodel <- glm(foul.home ~ foul.diff, 
                  family = binomial, 
                  data = mydata)
summary(bonusmodel)
