#Assignments in this doc: 
#Chapter 3
#3.7.2 ("Guided Exercises"): #1. 

#3.7.2 #1: 

#We generate "plain old" binomial data to compare against the beta-binomial distribution
#Defined by the exercise
mydata <- rbinom(n=1000, size=10,
                 p=0.8)

hist(mydata, breaks=5, xlim = c(2, 15))

#We want 1,000 instances of beta-binom values so we generate the 1,000 p_i's
p_is <- rbeta(n=1000, shape1 = 4, shape2 = 1)

#We define our function to use with sapply
#Where the input/value from above becomes a binom parameter
betabinom <- function(x) {  
  rbinom(n=1, size=10, p=x)}

#And execute it
ourdata <- sapply(p_is, betabinom)
hist(ourdata, xlim=c(-1, 12))

#And we grab some statistics to compare and look at: 

#The binomial stats:
mean(mydata)
sd(mydata)
min(mydata)
max(mydata)

#The beta binomial stats:
mean(ourdata)
sd(ourdata)
min(ourdata)
max(ourdata)

#And to get more of an idea: 
#What do the means look like?

#BINOM: 
allbm <- function(x) {  
  mydata <- rbinom(n=1000, size=10, p=0.8)
  mean(mydata)}

our_b_ms <- sapply(1:1000, allbm)
hist(our_b_ms, main='Binomial Dist, Means')
y <- mean(our_b_ms)
abline(v=y, col='red')

#BETABINOM: 
allbbm <- function(x) {  
  p_is <- rbeta(n=1000, shape1 = 4, shape2 = 1)
  ourdata <- sapply(p_is, betabinom)
  mean(ourdata)}

our_bb_ms <- sapply(1:1000, allbbm)
hist(our_bb_ms, main='Beta Binomial Dist, Means')
y <- mean(our_bb_ms)
abline(v=y, col='red')

#What do the SDs look like?

#BINOM: 
allbsd <- function(x) {  
  mydata <- rbinom(n=1000, size=10, p=0.8)
  sd(mydata)}

our_b_sd <- sapply(1:1000, allbsd)
hist(our_b_sd, main='Binomial Dist, Standard Deviations')
y <- mean(our_b_sd)
abline(v=y, col='blue')

#BETABINOM: 
allbbsd <- function(x) {  
  p_is <- rbeta(n=1000, shape1 = 4, shape2 = 1)
  ourdata <- sapply(p_is, betabinom)
  sd(ourdata)}

our_bb_sds <- sapply(1:1000, allbbsd)
hist(our_bb_sds, main='Beta Binomial Dist, Standard Deviations')
y <- mean(our_bb_sds)
abline(v=y, col='blue')