#Assignments in this doc: 
#Chapter 10: Longitudinal 2-level models and >2 level models
#Construct Figures: 10.1, 10.2, 10.3, 10.4
#Replicate analyses in: §10.5 (Models on p. 20/61 & p. 24/61)

##########################
#Use R . . . to construct figures 10.1-4
##########################

#We read in the data: 
mydata <- read.csv("https://raw.githubusercontent.com/proback/BYSH/master/data/seeds2.csv")

#Take a look at it
#View(mydata)
names(mydata)

#We reshape the data from WIDE to LONG
#We tell it which columns to collapse
longdata <- reshape(mydata, varying = c("hgt13","hgt18","hgt23","hgt28"), 
                    timevar = "time", idvar = "plant", direction = "long", sep = "")

#take a look: 
#View(longdata)

#compute the new time variable
longdata$time13 <- longdata$time

#Making figure 10.1
library(ggplot2)

#To make plots we generate means by plant
means <- aggregate(hgt ~  plant, longdata, mean)

#take a look
#View(means)

names(means) <- c("plant", "meanhgt")

#join the plant means back to the other relevant data: 
library(dplyr)
leftdata <- left_join(means, longdata, by='plant')
#View(leftdata)

#for title display we do this: 
leftdata$speciesbox <-  factor(leftdata$species,
                                 labels = c("Coneflowers", "Leadplants"))

#and we make 10.1:
p <- ggplot(leftdata, aes(y = meanhgt, x = soil)) +
  geom_boxplot() + 
  facet_wrap(~ speciesbox, scales = 'free', labeller = "label_value", nrow=2)
p <- p + xlab("Box plots of plant heights")  + 
  ylab("Plant Height (mm)") + 
  ggtitle("(a)")
#have a look: 
#p

q <- ggplot(leftdata, aes(y = meanhgt, x = sterile)) +
  geom_boxplot() + 
  facet_wrap(~ speciesbox, scales = 'free', labeller = "label_value", nrow=2)
q <- q + xlab("Box plots of plant heights")  + 
  ylab("Plant Height (mm)") + 
  ggtitle("(b)")
#and here: 
#q

#We display multiple ggplots on one panel with this library: 
#install.packages("ggpubr")
library(ggpubr)

ggarrange(p, q, 
          labels = c("  By soil type", "Sterilized yes/no"),
          ncol = 2, nrow = 1)

#Making figure 10.2: 
#we make the spaghetti plot: 
pq <- ggplot(leftdata, aes(x=time, y=hgt)) + 
  geom_line(alpha = 0.5) + guides(colour=FALSE) + 
  ylab("Y") + aes(colour = factor(plant))+ 
  facet_wrap(~ speciesbox, ncol=2) +
  ylab("Plant height (mm)") +
  xlab("Days since seeds planted") 

pq + 
  geom_smooth(method="loess", colour="black", se=TRUE)

#Lattice plot time: 
#Making figure 10.3: 

#We require a random sample of 24 of the leadplants, conducted like last week: 
leftl <- leftdata[ which(leftdata$species == 'L'), ]
leadids <- unique(leftl$plant)
ourrandleads <- leadids[sample(length(leadids),24)]

leaddata103 <- leftdata[which(leftdata$plant %in% ourrandleads), ]

#View(leaddata103)

#And we make the plot
pq <- ggplot(leaddata103, aes(x=time, y=hgt)) + 
  ylab("Y") +  ylim(-4, 9) +
  scale_y_continuous(breaks = c(-3, 0, 3, 6)) +
  facet_wrap(~ plant, ncol=6, scales="free_x") +
  ylab("Plant height (mm)") +
  xlab("Time") +  
  theme(
    strip.text.x = element_blank()
  )

pq + 
  geom_smooth(method="lm", colour="blue", se=TRUE)

#another lattice plot
#by pots: 
#Constructing figure 10.4: 
pq <- ggplot(leftl, aes(x=time, y=hgt)) + 
  geom_line(alpha = 0.5) + guides(colour=FALSE) + 
  ylab("Y") + aes(colour = factor(plant))+ 
  facet_wrap(~ pot, ncol=7) +
  ylab("Plant height (mm)") +
  xlab("Days since seeds planted") +  
  #and we get rid of the id number titles above each separate graph panel
  theme(
    strip.text.x = element_blank()
  )

pq + 
  geom_smooth(method="loess", colour="black", se=FALSE)

#Chapter 10 ctd.: Longitudinal 2-level models and >2 level models

###Replicate analyses in: §10.5 (Models on p.20/61 & p. 24/61)

library(lme4)

#Data engineering again if necessary: 

#We reshape the data from WIDE to LONG
#We tell it which columns to collapse

#longdata <- reshape(mydata, varying = c("hgt13","hgt18","hgt23","hgt28"), 
#                    timevar = "time", idvar = "plant", direction = "long", sep = "")

#take a look: 
#View(longdata)

#compute the new time variable
longdata$time13 <- longdata$time - 13

#we filter out the plants with no data: 
present <- longdata[ which(longdata$hgt != 'NA'), ]
dim(longdata)
dim(present)

#Take a look to make sure it worked: 
#View(present)

#we note the indices
presentplants <- unique(present$plant)

#We make sure that the two are not equal: 
all.equal(unique(longdata$plant), presentplants)

#So we filter it: 
ourdata <- filter(longdata,plant %in% presentplants)
dim(ourdata)

#And we are specifically modeling the data of leadplants here: 
leaddata <- ourdata[ which(ourdata$species == 'L'), ]
dim(leaddata)

#real quick, the unconditional means model from 10.4
uncondmeans <- lmer(formula = hgt ~ 1 + (1 | plant) + (1 | pot), 
                    data=leaddata)
summary(uncondmeans)

#And the composite model from 10.4, needed for the comparison in 10.5: 
modelb <- lmer(formula = hgt ~ time13 + (time13 | plant) + (time13 | pot), 
               data=leaddata)
summary(modelb)

#The models from 10.5: 

#We engineer some indicator variables: 
leaddata$cult <- 0
leaddata$cult[leaddata$soil == 'CULT'] <- 1
leaddata$rem <- 0
leaddata$rem[leaddata$soil == 'REM'] <- 1
leaddata$strl <- 0
leaddata$strl[leaddata$sterile == 'Y'] <- 1

#And the model from 10.5 with Level Three covariates: 
level3model <- lmer(formula = hgt ~ time13 + strl + cult + rem + 
                      time13:strl + time13:cult + time13:rem + 
                      (time13 | plant) + (time13 | pot), 
                    data=leaddata)
summary(level3model)

#and we compare them: 
anova(level3model, modelb, test="Chisq")

#we encounter the boundary constraint issue and experiment with just removing the term: 
removedmodel <- lmer(formula = hgt ~ time13 + strl + cult + rem + 
                       time13:strl + time13:cult + time13:rem + 
                       (time13 | plant) + (1 | pot) + (0 + time13 | pot), 
                     data=leaddata)
summary(removedmodel)

#we consider a final revision
#assuming pot growth rate to be constant, 
#Thus removing the parameters for variance and its correlation with the other variance
revisedmodel <- lmer(formula = hgt ~ time13 + strl + cult + rem + 
                       time13:strl + time13:cult + time13:rem + 
                       (time13 | plant) + (1 | pot), 
                     data=leaddata)
summary(revisedmodel)

#And we compare this model to what we had above: 
anova(revisedmodel, level3model)