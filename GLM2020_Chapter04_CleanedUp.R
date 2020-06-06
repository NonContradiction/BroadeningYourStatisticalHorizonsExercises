#Assignments in this doc: 
#Chapter 4
#Problem 4.11.2 #2 (Elephant Mating)

#Problem 4.11.2 #2:

#Poisson distributions &c.

#We pull the data from the csv at the Github URL 
mydata <- read.csv("https://raw.githubusercontent.com/proback/BYSH/master/data/elephant.csv")

#Take a look at it: 
#View(mydata)

#Problem a: histogram

hist(mydata$MATINGS, 
     main="Histogram for Matings", 
     xlab="MATINGS", 
     border="slateblue2", 
     col="skyblue2",
     xlim=c(0,10),
     las=1, 
     breaks=9)

#Promising (there is preliminary evidence), because: 
#The variable is a count with a floor of 0
#The histogram shows that our data is right-skewed

#Problem b:

#We fit an OLS model
fit <- lm(mydata$MATINGS ~ mydata$AGE)

#Plot the data and a fit line: 
plot(mydata$AGE, mydata$MATINGS)
abline(fit)

#Look at the plots for our linear model: 
plot(fit)

#problem c: 

#we calculate the mean number per age
mymeans <- aggregate(mydata[, 2], list(mydata$AGE), mean)
#take the log
mymeans$logs <- log(mymeans$x)

#Visual spot-check:
#View(mymeans)

plot(mymeans$Group.1, mymeans$logs)

#problem d: 

#we fit a poisson model: 
modeld <- glm(formula = mydata$MATINGS ~ mydata$AGE, family=poisson)

#We print out the summary: 
summary(modeld)

#Here we exponentiate the coefficient
exp(modeld$coefficients[[2]])
#And interpret it: 
#As per p. 11/59, this means that an increase by 1 in AGE translates to an increase of 7%.

#problem e: 
#We want a 95% confidence interval for the slope: 

#Here are our model's coefficients: 
#summary(modeld)$coefficients

#We use the standard error . . .
stderr <- summary(modeld)$coefficients[4]

#looking for a confidence interval for this statistic, the slope for age: 
estimate <- modeld$coefficients[[2]]

#We use the std normal to give us bounds
lowbd <- estimate - 1.96*stderr
upbd <- estimate + 1.96*stderr

#Visual spot-check: 
#lowbd
#upbd

#And exponentiate them to get something more immediately meaningful: 
c(exp(lowbd), exp(upbd))

#problem f: 
#part i: Wald test

waldtype <- estimate / stderr
waldtype

#So we use the pnorm function to find the p-value for the standard normal distribution
#(the wald type statistic follows such a normal dist)
#By having pnorm tell us the area of the dist. under the curve and to the left of the opposite of the abs val
#Doubled because we are agnostic about whether we expected a greater or lesser value (2-tailed test)
2*pnorm(-abs(waldtype))
#Tiny p-value suggests a meaningful model

#part ii: drop in deviance test 
#We read off the null deviance: 
summary(modeld)

did <- 75.372-deviance(modeld)
#Examine it: 
did

#And we complete the drop in deviance test for significance and get a highly significant 0, 
#suggesting another meaningful model: 
pchisq(-abs(did), df=1)

#problem g:

#We compute the quadratic variable: 
mydata$AGE2 <- mydata$AGE**2

#And fit a second order model: 
modelg <- glm(formula = mydata$MATINGS ~ mydata$AGE + mydata$AGE2, family=poisson)

#Can examine it: 
#modelg

#Do we prefer the quadratic model to a linear one? 

#Wald test for significance: 
stderr <- summary(modelg)$coefficients[6]
estimate <- modelg$coefficients[[3]]
waldtype <- estimate / stderr
result <- 2*pnorm(-abs(waldtype))
result

#Drop in deviance test for significance: 
drop.in.dev<- modeld$deviance - modelg$deviance
diff.in.dev <- modeld$df.residual - modelg$df.residual
1-pchisq(drop.in.dev, diff.in.dev)

#Identical results means we do not prefer the second order model to the first

#problem h:
1-pchisq(modeld$deviance, modeld$df.residual)
#Our model is not great but probably still meaningful 

#problem i:
#Fit the model with a quasilikelihood: 
modeli <- glm(formula = mydata$MATINGS ~ mydata$AGE, family=quasipoisson)
summary(modeld)$coefficients #Estimated coefficients don't change
summary(modeli)$coefficients #the std. errors are 8% bigger
summary(modeli) #New estimated dispersion parameter: 1.157334

#Response to question i.iv: Our parameter > 1 suggests overdispersion--
#and when we correct that, by enlarging standard errors, 
#we expect to find that fewer coefficients count as significant
#(per BYSH Ch. 4 p. 35/59).