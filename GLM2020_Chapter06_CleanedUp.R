#Assignments in this doc: 
#Chapter 6

"""
Models in §6.5.6, §6.5.8. (data RR_Data_Hale.csv).
Fit the last model in model in §6.7.3 and the model with the
female:media interaction. Data yrbs09.sav - this is a SPSS
data file.
"""

#################
#PART ONE
#################

#We read in the data: 
mydata <- read.csv("https://raw.githubusercontent.com/proback/BYSH/master/data/RR_Data_Hale.csv")

#ANOVA below gives an error for having trained data on "different sizes" and can't compute DF's
#Because the reduced model uses everything, but the Black Pop. one has one missing record.
#So we take a line here to throw it out altogether

usedf <- subset(mydata, X..Pop..Black != 'NA')

#Visual spot-check
#View(usedf)

names(usedf)

#We build up to the models in sections 6.5.6 and 6.5.8

#6.5.3

#Initial model
model1 <- glm(formula = cbind(Yes.1st.Vote, No.1st.Vote) ~ Distance.from.RR, 
    family=binomial, data = usedf, na.action = na.exclude)
summary(model1)

#Adding a covariate: 
model2 <- glm(formula = cbind(Yes.1st.Vote, No.1st.Vote) ~ Distance.from.RR + X..Pop..Black, 
    family=binomial, data = usedf, na.action = na.exclude)
summary(model2)

#6.5.4: Sig tests 

#Wald test (much like last time): 
#The wald test statistic is estimate / std. err. 
black.est <- summary(model2)$coefficients[[3, 1]]
black.stderr <- summary(model2)$coefficients[[3, 2]]

wald.stat <- black.est/black.stderr

#And we compute the p-value from the Z-score
2*pnorm(-abs(wald.stat))
#It looks really significant!

#Drop in deviance test, comparing the two models on their respective degrees of freedom: 
anova(model1, model2, test="Chisq")

#6.5.5: Conf ints.
#confidence interval: 
#Like last time: 
#We use the std normal to give us bounds
lowbd <- black.est - 1.96*black.stderr
upbd <- black.est + 1.96*black.stderr

#We can print these out if we choose: 
#print(lowbd)
#print(upbd)

#And exponentiate them to something more intuitive: 
c(exp(lowbd), exp(upbd))

#Also exponentiate them given the coefficient of 10, 
#to represent an estimated correspondence to a 10% increase: 
c(exp(10*lowbd), exp(10*upbd))

#The chapter also suggests the profile likelihood method: 
exp(confint(model2))

#Section 6.5.6: Goodness of Fit (GOF)

#residual deviance test on chi-sq dist.: 
ouranova <- anova(model1, model2, test="Chisq")
our.n <- ouranova[[2]][2]
our.df <- ouranova[[1]][2]
1-pchisq(our.n, our.df)

#Add in an interaction term for those two covariates: 
model3 <- glm(formula = cbind(Yes.1st.Vote, No.1st.Vote) ~ Distance.from.RR + X..Pop..Black + Distance.from.RR:X..Pop..Black, 
              family=binomial, data = usedf, na.action = na.exclude)
summary(model3)

#Wald test, again: 

#The wald test statistic is estimate / std. err. 
black.est <- summary(model3)$coefficients[[4, 1]]
black.stderr <- summary(model3)$coefficients[[4, 2]]
summary(model3)$coefficients

wald.stat <- black.est/black.stderr
wald.stat

#And we compute the p-value from the Z-score
2*pnorm(-abs(wald.stat))

#Drop in deviance test, comparing the two models on their respective degrees of freedom: 
anova(model2, model3, test="Chisq")

#Goodness of fit?
ourmodel <- anova(model2, model3, test="Chisq")
our.n <- ourmodel[[2]][2]
our.df <- ourmodel[[1]][2]
our.n
our.df
1-pchisq(our.n, our.df)
# still lacks GOF.

#6.5.8
#The dispersion is greater than binomial (binomial variance = n*p*(1-p))
#So we use a quasi-binomial family, thus: 

model4 <- glm(formula = cbind(Yes.1st.Vote, No.1st.Vote) ~ Distance.from.RR + X..Pop..Black + Distance.from.RR:X..Pop..Black, 
              family=quasibinomial, data = usedf, na.action = na.exclude)
summary(model4)

#The p-values have changed! So we remove the interaction ter and refit, as the book suggests.
model5 <- glm(formula = cbind(Yes.1st.Vote, No.1st.Vote) ~ Distance.from.RR + X..Pop..Black, 
              family=quasibinomial, data = usedf, na.action = na.exclude)
summary(model5)

exp(confint(model5))
#1 is contained in the confidence interval for x..pop..black, which reflects the lack of significance above.

#################
#PART TWO
#################

#We read in the data: 
library(foreign)
risk2009.data = read.spss(file = "https://raw.githubusercontent.com/proback/BYSH/master/data/yrbs09.sav", to.data.frame = TRUE)

#View it if desired:
#View(risk2009.data)

names(risk2009.data)

#We pare it down to the requested features:
risk2009 <- risk2009.data[, c("Q2", "Q66", "Q81", "bmipct")]
#View(risk2009)

#We use complete case analysis, as the book requires
risk2009 <- na.omit(risk2009)

#Look at the dimensions
dim(risk2009)

#We encode the factors as the book dictates
print(levels(risk2009.data$Q2))
print(levels(risk2009.data$Q66))
print(levels(risk2009.data$Q81))

levels(risk2009$Q2) <- c(1, 0)
levels(risk2009$Q66) <- c(1, 0, 0, 0)
levels(risk2009$Q81) <-  c(0,0.5, 1,2,3,4,5)

#We turn them into numerics
risk2009$Q2<- as.numeric(as.character(risk2009$Q2))
risk2009$Q66<- as.numeric(as.character(risk2009$Q66))
risk2009$Q81<- as.numeric(as.character(risk2009$Q81))

#Spot check it: 
#View(risk2009)

#We build our initial model: 
model1 <- glm(formula = Q66 ~ Q2, family=binomial, data=risk2009)
summary(model1)

#We add a covariate: 
model2 <- glm(formula = Q66 ~ Q2 + bmipct, family=binomial, data=risk2009)
summary(model2)

#Here the book adds sport as a covariate and rejects it as insignificant

#Now we add media: 
model3 <- glm(formula = Q66 ~ Q2 + bmipct + Q81, family=binomial, data=risk2009)
summary(model3)

#We also look at the wald test for significance of this new term: 

#The WALD TEST statistic is estimate / std. err. 
media.est <- summary(model3)$coefficients[[4, 1]]
media.stderr <- summary(model3)$coefficients[[4, 2]]
#summary(model3)$coefficients
wald.stat <- media.est/media.stderr
wald.stat
#And we compute the p-value from the Z-score
2*pnorm(-abs(wald.stat))
#We found a stat that makes it marginally similar owing to our different data set
#the book, with its different data, found an insignificant wald stat
#And found it insignificant in the model summary.

#Finally we add a female-media interaction term: 
model4 <- glm(formula = Q66 ~ Q2 + bmipct + Q81 + Q2:Q81, family=binomial, data=risk2009)
summary(model4)

#We compare it to the model without the interaction term and find it significant
anova(model3, model4, test="Chisq")
#i.e., the effect size of media depends on the sex of the person