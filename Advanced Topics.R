library(R2jags)
library(MCMCvis)
library(lattice)

set.seed(65431)

# model definition
jags.mod.coin <- function(){
  Y ~ dbin(0.5,10) # our data model
  P8 <- ifelse(Y>7,1,0) # the probability of interest
}


jags.mod.fit.coin <- jags(data = list(), model.file = jags.mod.coin,
                          parameters.to.save = c('Y','P8'),n.chains=1,
                          DIC=FALSE, n.burnin=0,n.iter = 100)

# get numerical summary of the model
print(jags.mod.fit.coin)

# plot to see convergence
traceplot(jags.mod.fit.coin)


# convert into MCMC object
jagsfit.mcmc.coin <- as.mcmc(jags.mod.fit.coin)

# get numerical summary
summary(jagsfit.mcmc.coin)

# get traceplots
xyplot(jagsfit.mcmc.coin)

# get density estimate
densityplot(jagsfit.mcmc.coin)

MCMCtrace(jagsfit.mcmc.coin,
          params = 'Y', # parameter of interest
          type = 'density', # density plot
          ind = TRUE, # separate density lines for each chain
          pdf = FALSE) # plots are NOT exported into a pdf


MCMCtrace(jagsfit.mcmc.coin,
          params = 'P8',
          type = 'trace',
          ind = TRUE,
          pdf = FALSE)

jags.mod.fit.coin$BUGSoutput$summary[,1] # mean
jags.mod.fit.coin$BUGSoutput$summary[,3] # 2.5 percentile
jags.mod.fit.coin$BUGSoutput$summary[,7] # 97.5 percentile

############## Exercise 1 ######################
### Q1. increase of the iteration to 100.000 with chain=1
# remark: when increase the number of iteration, the parameters
# tend to increase too
jags.mod.fit.coin <- jags(data = list(), model.file = jags.mod.coin,
                          parameters.to.save = c('Y','P8'),n.chains=1,
                          DIC=FALSE, n.burnin=0,n.iter = 100000)

# get numerical summary of the model
print(jags.mod.fit.coin)

# plot to see convergence
traceplot(jags.mod.fit.coin)


# convert into MCMC object
jagsfit.mcmc.coin <- as.mcmc(jags.mod.fit.coin)

# get numerical summary
summary(jagsfit.mcmc.coin)

# get traceplots
xyplot(jagsfit.mcmc.coin)

# get density estimate
densityplot(jagsfit.mcmc.coin)

MCMCtrace(jagsfit.mcmc.coin,
          params = 'Y', # parameter of interest
          type = 'density', # density plot
          ind = TRUE, # separate density lines for each chain
          pdf = FALSE) # plots are NOT exported into a pdf


MCMCtrace(jagsfit.mcmc.coin,
          params = 'P8',
          type = 'trace',
          ind = TRUE,
          pdf = FALSE)

jags.mod.fit.coin$BUGSoutput$summary[,1] # mean
jags.mod.fit.coin$BUGSoutput$summary[,3] # 2.5 percentile
jags.mod.fit.coin$BUGSoutput$summary[,7] # 97.5 percentile


### Q2. increase of the iteration to 300.000 with chain=2
jags.mod.fit.coin <- jags(data = list(), model.file = jags.mod.coin,
                          parameters.to.save = c('Y','P8'),n.chains=2,
                          DIC=FALSE, n.burnin=0,n.iter = 300000)

# convert into MCMC object
jagsfit.mcmc.coin <- as.mcmc(jags.mod.fit.coin)

# get numerical summary
summary(jagsfit.mcmc.coin)

### Q2. increase of the iteration to 300.000 with chain=3
jags.mod.fit.coin <- jags(data = list(), model.file = jags.mod.coin,
                          parameters.to.save = c('Y','P8'),n.chains=3,
                          DIC=FALSE, n.burnin=0,n.iter = 3000000)

# convert into MCMC object
jagsfit.mcmc.coin <- as.mcmc(jags.mod.fit.coin)

# get numerical summary
summary(jagsfit.mcmc.coin)

## Q3
set.seed(65431)

# model definition
jags.mod.trial <- function(){
  Y ~ dbin(0.7,30) # our data model
  P8 <- ifelse(Y <= 15,1,0) # the probability of interest
}


jags.mod.fit.trial <- jags(data = list(), model.file = jags.mod.trial,
                          parameters.to.save = c('Y','P8'),n.chains=1,
                          DIC=FALSE, n.burnin=0,n.iter = 1000)

# convert into MCMC object
jagsfit.mcmc.trial <- as.mcmc(jags.mod.fit.trial)

# get numerical summary
summary(jagsfit.mcmc.trial)


## Q4
# mean 1 and standard deviation 2
# model definition
jags.mod.norm <- function(){
  x ~ dnorm(1,1/4) # our data model
  Y <- x^3
}


jags.mod.fit.norm <- jags(data = list(), model.file = jags.mod.norm,
                           parameters.to.save = c('x','Y'),n.chains=1,
                           DIC=FALSE, n.burnin=0,n.iter = 1000)

# convert into MCMC object
jagsfit.mcmc.norm <- as.mcmc(jags.mod.fit.norm)

# get numerical summary
summary(jagsfit.mcmc.norm)


####### Drug example without data #################
# model
jags.mod.drug <- function(){
  # prior for the unknown parameter
  theta~dbeta(9.2,13.8)
  # the data model
  y~dbin(theta,20)
  #quantity of interest
  P.crit <- ifelse(y>=15,1,0)
}

# fit model
jags.mod.fit.drug <- jags(data = list(),n.iter = 10000,DIC = FALSE,
                          parameters.to.save = c('theta','y','P.crit'),
                          model.file = jags.mod.drug,n.chains = 1)


# display the numerical summaries
print(jags.mod.fit.drug)

# convert into MCMC object for more visualisation tools
jagsfit.mcmc.drug <- as.mcmc(jags.mod.fit.drug)

#get the density plots of the monitored nodes
densityplot(jagsfit.mcmc.drug)

# plot histogram of the discrete distribution
library(jagsplot)
jags.hist(jags.mod.fit.drug, which.param = c('theta','y','P.crit'))

# exercises
# 1- edit the model code using uniform(0,1) prior on the response 
# model
jags.mod.drug2 <- function(){
  # prior for the unknown parameter
  theta~dunif(0,1)
  # the data model
  y~dbin(theta,20)
  #quantity of interest
  P.crit <- ifelse(y>=15,1,0)
}

# fit model
jags.mod.fit.drug2 <- jags(data = list(),n.iter = 10000,DIC = FALSE,
                          parameters.to.save = c('theta','y','P.crit'),
                          model.file = jags.mod.drug2,n.chains = 1)


# display the numerical summaries
print(jags.mod.fit.drug2)

# convert into MCMC object for more visualisation tools
jagsfit.mcmc.drug2 <- as.mcmc(jags.mod.fit.drug2)

#get the density plots of the monitored nodes
densityplot(jagsfit.mcmc.drug2)

# plot histogram of the discrete distribution
library(jagsplot)
jags.hist(jags.mod.fit.drug2, which.param = c('theta','y','P.crit'))

################## predictive model #######################################

jags.mod.drug3 <- function(){
  r ~ dbin(theta,n) # prior
  theta ~ dbeta(a,b) # likelihood
  r.pred ~ dbin(theta,m) # predictive distribution
  P.crit <- ifelse(r.pred>=ncrit,1,0) # probability of interest
}

# data
a=9.2 #first parameter of the prior
b=13.8  # second parameter of the prior
r=15 # our data
n=20 # patient number
m=40 # future patient number
ncrit=25 # threshold for the future trial

jags.data.drug3 <- list('a','b','r','n','m','ncrit')

# Parameters we want to monitor
jags.param.drug3 <- c('theta','r.pred','P.crit')

# Specify initial values
jags.inits1 <- list('theta'=0.7,'r.pred'=20)
jags.inits2 <- list('theta'=0.5,'r.pred'=28)
jags.inits.drug3 <- list(jags.inits1,jags.inits2)


## Fit the JAGS model using two chains
jags.mod.fit.drug3 <- jags(data = jags.data.drug3,inits = jags.inits.drug3,
                           parameters.to.save = jags.param.drug3,n.chains = 2,
                           n.iter = 10000,model.file = jags.mod.drug3)

# display the numerical summaries
print(jags.mod.fit.drug3)

## convergence checking
# step1 traceplots (selection of parameters<10)
# step2 scale reduction factors : gelman function

# convert into MCMC object for more visualisation tools
jagsfit.mcmc.drug3 <- as.mcmc(jags.mod.fit.drug3)

#trace plots
MCMCtrace(jagsfit.mcmc.drug3,params = c('theta','r.pred'),
           type = 'trace',ind = TRUE,pdf = FALSE )

# scale reduction factors : gelman function
gelman.diag(jagsfit.mcmc.drug3)


# get the numerical summaries
summary(jagsfit.mcmc.drug3)

#get the density/kernel plots of the monitored nodes
densityplot(jagsfit.mcmc.drug3)

# plot histogram of the discrete distribution
library(jagsplot)
jags.hist(jags.mod.fit.drug3, which.param = c('theta','y','P.crit'))



############### Edit the model code to specify a Uniform(0, 1) prior 
################# on the response rate theta

## predictive model

jags.mod.drug4 <- function(){
  r ~ dbinom(theta,n) # prior of the response rate
  theta ~ dunif(0,1) # likelihood
  r.pred ~ dbinom(theta,m) # predictive distribution
  P.crit <- ifelse(r.pred>=ncrit,1,0) # probability of interest
}

# data
#a=9.2 #first parameter of the prior
#b=13.8  # second parameter of the prior
r=15 # our data
n=20 # patient number
m=40 # future patient number
ncrit=25 # threshold for the future trial

jags.data.drug4 <- list('r','n','m','ncrit')

# Parameters we want to monitor
jags.param.drug4 <- c('theta','r.pred','P.crit')

# Specify initial values
jags.inits1 <- list('theta'=0.7,'r.pred'=20)
jags.inits2 <- list('theta'=0.5,'r.pred'=28)
jags.inits.drug4 <- list(jags.inits1,jags.inits2)


## Fit the JAGS model using two chains
jags.mod.fit.drug4 <- jags(data = jags.data.drug4,inits = jags.inits.drug4,
                           parameters.to.save = jags.param.drug4,n.chains = 2,
                           n.iter = 10000,model.file = jags.mod.drug4)

# display the numerical summaries
print(jags.mod.fit.drug4)

# convert into MCMC object for more visualisation tools
jagsfit.mcmc.drug4 <- as.mcmc(jags.mod.fit.drug4)

# get the numerical summaries
summary(jagsfit.mcmc.drug4)

#get the density/kernel plots of the monitored nodes
densityplot(jagsfit.mcmc.drug4)

# plot histogram of the discrete distribution
library(jagsplot)
jags.hist(jags.mod.fit.drug4, which.param = c('theta','y','P.crit'))

########################### Practical####################################
# Question1
jags.mod.comp <- function(){
  for (i in 1:10) {
    x[i] ~ dgamma(4,2/5)
  }
  
  cs[1] <- x[1]
  for (i in 2:10) {
    cs[i] <- cs[i-1] + x[i]
    
    done <- ifelse(cs[i]<100,1,0)
    
  }
  
  success <- ifelse(cs[10]<100,1,0)
  
  finish <- sum(done)
}
######################################
N <- 10

jags.mod.comp <- function() {
  for (i in 1:N) {
    x[i] ~ dgamma(4, 2/5)  # Generate random variables
  }
  
  cs[1] <- x[1]  # Initialize the cumulative sum
  for (i in 2:N) {
    cs[i] <- cs[i - 1] + x[i]  # Compute cumulative sum
  }
  
  for (i in 1:N) {
    done[i] <- ifelse(cs[i] < 100, 1, 0)  # Check if cs[i] < 100
  }
  
  success <- ifelse(cs[10] < 100, 1, 0)  # Final success check
  finish <- sum(done)  # Sum up done values
}

jags.mod.fit <- jags(data = list('N'),
                     parameters.to.save = c('success','finish')
                     ,n.iter = 10000,n.chains = 2,n.burnin = 5000,
                     model.file = jags.mod.comp,DIC = FALSE)

print(jags.mod.fit)

# convert to MCMC
jags.mcmc <- as.mcmc(jags.mod.fit)

summary(jags.mcmc)

## convergence checking
# step1 traceplots (selection of parameters<10)
# step2 scale reduction factors : gelman function

#trace plots
MCMCtrace(jags.mcmc,params = c('success','finish'),
          type = 'trace',ind = TRUE,pdf = FALSE )

# scale reduction factors : gelman function
gelman.diag(jags.mcmc)

############################## Question 2 problem sheet 1 #####################
library(R2jags)
model <- function(){
  pS ~ dbin(pi,N) # likelihood of number of success
  for (i in 1:pS) {
    
    sT[i] ~ dexp(theta) # likelihood of survival time
  }
  pi ~ dunif(0,1) # prior of success
  theta ~ dgamma(0.001,0.001) # prior of survival time
  surv.t <- pi/theta # expected lifetime
}

# parameters
params <- c('pi','theta','surv.t')

# initial values
inits1 <- list(pi=0.5,theta=0.1)
inits2 <- list(pi=0.7,theta=0.8)
inits <- list(inits1,inits2)
#data
pS <- 9
sT <- c(2, 3,4, 4, 6, 7, 9, 10, 12)
N <- 12
data <- list(pS = pS, sT = sT, N = N)

jags.heart.mod <- jags(data = data,inits = inits,parameters.to.save = params,
                       model.file = model,n.iter = 10000,n.burnin = 1000,
                       n.chains = 2)

jags.heart.mod.mcmc <- as.mcmc(jags.heart.mod)

summary(jags.heart.mod.mcmc)

###### convergence checking
# traceplot
MCMCtrace(jags.heart.mod.mcmc,params = params,type = 'trace',ind = T,pdf = F)

## density plot
MCMCtrace(jags.heart.mod.mcmc,params = params,type = 'density',ind = T,pdf = F)

# gelman diagnostic
gelman.diag(jags.heart.mod.mcmc)


###############################################################
{r}
beta <- jags.model.fit$BUGSoutput$sims.list$beta
lambda <- jags.model.fit$BUGSoutput$sims.list$lambda
tau <- jags.model.fit$BUGSoutput$sims.list$Tau

p <- cbind(beta,lambda,tau)
colnames(p) <- c('beta','lambda','tau')
p.median <- p[,order(apply(p, 2, FUN = median))]
boxplot(p.median,outline=FALSE)
###############################################################
x <- c( 1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0,
        8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0,
        13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5)

Y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
      2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
      2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57)
N = 27
jags.data <- list("x","Y", "N")
jags.mod <- function(){
  for (i in 1 : N) {
    Y[i] ~ dnorm(mu[i],tau)
    y.pred[i] ~ dnorm(mu[i],tau) # extra node
    mu[i] <- alpha-beta*gamma^x[i]
  }
  # priors
  alpha ~ dunif(0,100)
  beta ~ dunif(0,100)
  gamma ~ dunif(0,1.0)
  tau ~ dgamma(0.001,0.001)
  sigma <- 1/sqrt(tau)
}
# We will only monitor the y.pred node
jags.param <- c("y.pred")
# Initial values
inits1 <- list(alpha = 1, beta = 1, tau=1, gamma = 0.9)
inits2 <- list(alpha = 10, beta = 3, tau=5, gamma = 0.1)
jags.inits <- list(inits1, inits2)
# In theory we would need to initialise y.pred, but if we don't specify that
# JAGS will just generate initial values for us
# Fit JAGS
jags.mod.fit <- jags(data = jags.data, inits = jags.inits,
                     parameters.to.save = jags.param, n.chains = 2, n.iter = 10000,
                     n.burnin = 1000,n.thin=1,model.file = jags.mod)
# Look at outcome - point/interval estimates
print(jags.mod.fit)
# Plot the posterior prediction
pos <- substr(rownames(jags.mod.fit$BUGSoutput$summary),1,6)=='y.pred'
# extract posterior mean for mu
mu <- jags.mod.fit$BUGSoutput$summary[pos,1]
# extract 2.5 percentile of the posterior
lcr <- jags.mod.fit$BUGSoutput$summary[pos,3]
# extract 97.5 percentile of the posterior
ucr <- jags.mod.fit$BUGSoutput$summary[pos,7]
# add these to a dataframe
df <- data.frame(x=x,y=Y,mu=mu,lower=lcr,upper=ucr)
ggplot(data=df) +
  geom_point(aes(x=x,y=y))+
  geom_line(aes(x=x,y=mu))+
  geom_line(aes(x=x,y=lower),linetype = "dashed",colour="red")+
  geom_line(aes(x=x,y=upper),linetype = "dashed",colour="red")
# By using y.pred we get the correct posterior uncertainty.
# Notice how the intervals on the first plot contain the real datapoints,
# which indicates a good model fit (generally we are happy if it contains
# about 95% of the datapoints

###################
############
library(kernlab)
library(e1071)
library(caret)
# SVM (sigmoid kernel)
# fit the model
svm.fit.sigmoid <- svm(ytrain~X1+X2, data = df, kernel = "sigmoid")

print(svm.fit.sigmoid)
# plot
plot(svm.fit.sigmoid,df)

# Test model on testing data
ytestPred.sigmoid <- predict(svm.fit.sigmoid, newdata=xtest)

# Ensure predictions match true levels
ytestPred.sigmoid <- factor(ytestPred.sigmoid, levels = levels(df$ytrain))
ytest <- factor(ytest, levels = levels(df$ytrain))

confusionMatrix(ytestPred.sigmoid, ytest) # predicted/true

### SVM (polynomial kernel)
# fit the model
svm.fit.poly <- svm(ytrain~X1+X2, data = df, kernel = "polynomial")

print(svm.fit.poly)
# plot
plot(svm.fit.poly,df)

# Test model on testing data
ytestPred.poly <- predict(svm.fit.poly, newdata=xtest)
# yTestPred <- mdl %>% predict(xTest) 

# Ensure predictions match true levels
ytestPred.poly <- factor(ytestPred.poly, levels = levels(df$ytrain))
ytest <- factor(ytest, levels = levels(df$ytrain))

confusionMatrix(ytestPred.poly, ytest) # predicted/true
####################
