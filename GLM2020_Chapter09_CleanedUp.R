#Assignments in this doc: 
#Chapter 9: 2 Level Longitudinal Data

"""
Construct Figures: 9.2, 9.4, 9.7
Replicate analyses in:
§9.5.2 and §9.5.3. 
Construct Figures: 9.18 - Model C (in §9.6.1)
Replicate analyses in: §9.6.3
"""

#Chapter 9: Longitudinal 2-level models

### Construct Figures: 9.2, 9.4, 9.7 Use R and SAS (proc mixed) to replicate analyses in:  §9.5.2 and §9.5.3.

#################
#Prep Data / Early Chapter Work
#################

#We read in the data: 
mydata <- read.csv("https://raw.githubusercontent.com/proback/BYSH/master/data/chart_wide_condense.csv")

#Take a look at it
#View(mydata)
names(mydata)

##########################
#Data Org in 9.3
##########################

#The book uses the lmer function so we do too: 
#install.packages("lme4")
library(lme4)

#We reshape the data from WIDE to LONG
#We tell it which columns to collapse
longdata <- reshape(mydata, varying = c("MathAvgScore.0", "MathAvgScore.1", "MathAvgScore.2"), 
                    timevar = "year08", idvar = "schoolid", direction = "long", sep = ".")
#View(longdata)

library(ggplot2)

#To make plots we generate means by school
means <- aggregate(MathAvgScore ~  schoolid, longdata, mean)
#View(means)
names(means) <- c("schoolid", "MeanScore")

library(dplyr)
leftdata <- left_join(means, longdata, by='schoolid')

#View(leftdata)

#We grab just the columns we want to work with
names(leftdata)
select<-c("schoolid", "MeanScore", "MathAvgScore", "urban", "charter")
selectedcols <- leftdata[,select]

names(selectedcols)
#View(selectedcols)

#For labeling our graphs we turn binary columns into factors 
selectedcols$urbanbox <-  factor(selectedcols$urban,
                                 labels = c("rural", "urban"))

selectedcols$charterbox <-  factor(selectedcols$charter,
                                 labels = c("public non-charter", "charter"))

#And we make sure that it worked: 
#View(selectedcols)

#We make Figure 9.2 the box plots

#First Box Plot
selectedcols$charterboxorder <- ordered(selectedcols$charterbox, levels = c("charter", "public non-charter"))
p <- ggplot(selectedcols, aes(y = as.factor(charterboxorder), x = MeanScore)) +
  geom_boxplot()
p <- p + xlab("Mean Math Scores by School")  + 
  ylab(" ") + 
  ggtitle("(a)")
#Reshape the graph viewer in RStudio to desired proportions
p

#Second Box Plot
p <- ggplot(selectedcols, aes(y = as.factor(urbanbox), x = MeanScore)) +
  geom_boxplot()
p <- p + xlab("Mean Math Scores by School")  + 
  ylab(" ") +
  ggtitle("(b)")
p

#Lattice plot
#We use the long data set again, since we don't want means
#View(longdata)

#We want all the data from "the first" 24 schools so we get that list
theschoools <- levels(longdata$schoolid)
theschoools <- theschoools[1:24]

#and subset our data from those 24
mysubset95 <- longdata[ which(longdata$schoolid %in% theschoools), ]
#View(mysubset95)

#And make the plot
q <- ggplot(mysubset95, aes(x=year08, y = MathAvgScore)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ schoolid, ncol=6) +
  ylab("Math Score") +
  xlab("Years since 2008") +
  ggtitle(" ")+ 
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
q

#Making figure 9.7
#Which uses a random sample from public schools and from charter schools

#We set up the charter factor and order it as in the graph
longdata$charterbox <-  factor(longdata$charter,
                                   labels = c("public non-charter", "charter"))
longdata$charterboxorder <- ordered(longdata$charterbox, levels = c("charter", "public non-charter"))

#We take a random sample of 60 from each category
longcharter <- longdata[ which(longdata$charter == 1), ]
longpublic <- longdata[ which(longdata$charter == 0), ]

schools_c <- unique(longcharter$schoolid)
schools_c <- schools_c[sample(length(schools_c),60)]

schools_p <- unique(longpublic$schoolid)
schools_p <- schools_p[sample(length(schools_p),60)]

#Combine them and filter it down
schools_r <- unlist(list(schools_c, schools_p))
longdata97 <- longdata[ which(longdata$schoolid %in% schools_r), ]

#And we make the plot
pq <- ggplot(longdata97, aes(x=year08, y=MathAvgScore)) + 
  geom_line(alpha = 0.5) + guides(colour=FALSE) + xlab("Observation Time Point") +
  ylab("Y") + aes(colour = factor(schoolid))+ 
  facet_wrap(~ charterboxorder, ncol=2) +
  ylab("Math Score") +
  xlab("Years since 2008") 

pq + 
  geom_smooth(method="loess", colour="black", se=TRUE)

##########################
#Use R . . . to replicate analyses in: §9.5.2
##########################

names(longdata)

#We buld our multilevel model, much like last time
multilevels <- lmer(formula = MathAvgScore ~ year08 + (year08 | schoolid), data=longdata)
summary(multilevels)

##########################
#Use R . . . to replicate analyses in: §9.5.3. 
##########################

#We buld our quadratic time model 
#Engineer a couple of new columns

longdata$timec <- longdata$year08 - 1
longdata$timec2 <- longdata$timec*longdata$timec
#View(longdata)

quadlevels <- lmer(formula = MathAvgScore ~ timec + timec2 + (1 | schoolid), data=longdata)
summary(quadlevels)

#################
#Construct Figures: 9.18 - Model C (in §9.6.1)
#Replicate analyses in: §9.6.3
#################

#We read in the data: 
#mydata <- read.csv("https://raw.githubusercontent.com/proback/BYSH/master/data/chart_wide_condense.csv")

#Take a look at it
#View(mydata)
names(mydata)

##########################
#Data Org in 9.3, retained from above, still needed
##########################

#The book uses the lmer function so we do too: 
library(lme4)
library(ggplot2)
library(dplyr)

#We reshape the data from WIDE to LONG
#We tell it which columns to collapse
longdata <- reshape(mydata, varying = c("MathAvgScore.0", "MathAvgScore.1", "MathAvgScore.2"), 
                    timevar = "year08", idvar = "schoolid", direction = "long", sep = ".")
#View(longdata)

#We fit models B and C for figure 9.18
#For labeling our graphs we turn binary columns into factors 
longdata$urbanbox <-  factor(longdata$urban,
                             labels = c("rural", "urban"))

longdata$charterbox <-  factor(longdata$charter,
                               labels = c("public non-charter", "charter"))

names(longdata)

#We buld our 9.6 model C, a composite of Level One and Level Two models: 
multilevels <- lmer(formula = MathAvgScore ~ charter + year08 + charter:year08 + (year08 | schoolid), 
                    data=longdata)
summary(multilevels)

#We also need Model B from last time for the figure: 
longdata$timec <- longdata$year08 - 1
longdata$timec2 <- longdata$timec*longdata$timec
#View(longdata)
quadlevels <- lmer(formula = MathAvgScore ~ timec + timec2 + (1 | schoolid), data=longdata)
summary(quadlevels)

#We fashion some data to predict on for a curve to graph 
#Give it the necessary structure: 
preddata <- longdata[0,]

#generate the data: 
for (yeari in 0:2){
  #unconditional
  newrow <- data.frame(1, 12345, "WalterWinchell", 1, 0, .5, .5, .5, yeari, 666, "fixthis", "fixthis", 0, 0)
  names(newrow) <- names(preddata)
  preddata <- rbind(preddata, newrow)
  #public
  newrow <- data.frame(1, 12345, "WalterWinchell", 0, 0, .5, .5, .5, yeari, 666, "fixthis", "fixthis", 0, 0)
  names(newrow) <- names(preddata)
  preddata <- rbind(preddata, newrow)  
  #charter
  newrow <- data.frame(1, 12345, "WalterWinchell", 0, 1, .5, .5, .5, yeari, 666, "fixthis", "fixthis", 0, 0)
  names(newrow) <- names(preddata)
  preddata <- rbind(preddata, newrow)
}

#and for graphing: 
preddata$urbanbox <-  factor(preddata$urban,
                             labels = c("rural", "urban"))

preddata$charterbox <-  factor(preddata$charter,
                               labels = c("public non-charter", "charter"))
preddata$timec <- preddata$year08 - 1
preddata$timec2 <- preddata$timec*preddata$timec
preddata$charterboxorder <- ordered(preddata$charterbox, levels = c("charter", "public non-charter"))

#We have a look: 
#View(preddata)

#We predict the values to graph for the appropriate rows: 
B_res <- predict(quadlevels, preddata, allow.new.levels = TRUE)
C_res <- predict(multilevels, preddata, allow.new.levels = TRUE)
relevantresults <- c(B_res[1], C_res[2], C_res[3], B_res[4], C_res[5], C_res[6], B_res[7], C_res[8], C_res[9])

#And affix them to our data set
preddata <- cbind(preddata, results = relevantresults)
preddata <- cbind(preddata, which_model = c(rep(c("B", "C", "C"), 3)))

#and take a look: 
#View(preddata)

#And we make the plot:
pq <- ggplot(preddata, aes(x=year08, y=results, group=charterboxorder)) + 
  geom_line(aes(linetype=charterboxorder)) + guides(colour=FALSE) + 
  xlab("Years since 2008") +
  ylab("Predicted Math Score") + 
  aes(colour = factor(schoolid))+ 
  facet_wrap(~ which_model, labeller = "label_both", strip.position = "top") +
  ggtitle("Fitted growth curves for Models B and C") +
  ylim(639, 661) +
  labs(shape="", linetype='')+
  geom_point(color="black", size=3, aes(shape=charterboxorder)) 
pq

#And the analysis from 9.6.3 is here: 
#Model F
modelF <- lmer(formula = MathAvgScore ~ charter + year08 + schPctfree + schPctsped + urban +
                 urban:year08 + schPctsped:year08 + charter:year08 + (year08 | schoolid), 
               data=longdata)
summary(modelF)