library(mnormt)
library(ggplot2)
library(geoR)
library(reshape2)
###
GPmean <- function(x,a=0,b=0,c=0){
  a+b*x+c*x*x
}


### covariation functions

## Exponential cov
GPexpcov <- function(x1,x2,sigma2=1,phi=.1,p=2){
  sigma2*(exp((-abs(x1-x2)^p)/phi))
}


## Matern
GPMatern <- function(x1,x2,sigma2=1,phi=.1,nu=1.5){
  cov <- sigma2*(2^(1-nu)/gamma(nu))*(sqrt(2*nu)*abs(x1-x2)/phi)^nu*
    besselK(sqrt(2*nu)*abs(x1-x2)/phi,nu)
  cov[is.na(cov)] <- sigma2 # if x1=x2, get NaN
  return(cov)
}


h <- seq(from = 0, to = 1, by = 0.01)
compare_covs <- data.frame(h = h,
                           Exponential = GPexpcov(h, 0, phi = 0.25, p = 1),
                           Exp15 = GPexpcov(h, 0, phi = 0.25, p = 1.5),
                           SquaredExp = GPexpcov(h, 0, phi = 0.25, p = 2),
                           Matern32 = GPMatern(h, 0, phi = 0.25),
                           Matern52 = GPMatern(h, 0, phi = 0.25, nu = 5/2))



plot_data <- melt(compare_covs, id.vars = 'h')
ggplot(data = plot_data, aes(x = h, y = value, col = variable)) +
  geom_line() +
  ylim(0,1) +
  labs(x = 'Distance', y = 'Correlation', col = 'Type')

############ Unconstrained sampling

x <- seq(from = 0, to = 9.9, by = 0.1) # 100 points

mu <- GPmean(x, a = 0, b = 0, c = 0) # assuming zero mean

covmat <- matrix(nrow=length(x), ncol=length(x))

for (i in 1:length(x)){
  for (j in i:length(x)){
    covmat[i,j] <- GPexpcov(x[i], x[j], phi=1, p=2)
    covmat[j,i] <- covmat[i,j] # we know the matrix is symmetric
  }
}

q <- 5 # number of samples
set.seed(1234)
realisations <- rmnorm(q, mu, covmat)


covmat <- matrix(nrow=length(x), ncol=length(x))

for (i in 1:length(x)){
  for (j in i:length(x)){
    covmat[i,j] <- GPexpcov(x[i], x[j], phi=1, p=1.99)
    covmat[j,i] <- covmat[i,j]
  }
}

realisations <- rmnorm(q, mu, covmat)

##
df <- data.frame(cbind(as.matrix(x),t(realisations)))

names(df) <- c('x','realisation_1','realisation_2','realisation_3','realisation_4','realisation_5')

unconstrained_plot <- ggplot(df) + geom_line(aes(x=x,y=realisation_1)) +
  geom_line(aes(x=x,y=realisation_2),colour='red') +
  geom_line(aes(x=x,y=realisation_4),colour='blue') +
  geom_line(aes(x=x,y=realisation_5),colour='orange') +
  geom_line(aes(x=x,y=realisation_3),colour='green')+
  labs(y = 'y')

unconstrained_plot



#############################
library(geoR)
library(ggplot2)
library(viridis)
x1 <- seq(from = 0, to = 10, by = 1)
x2 <- seq(from = 0, to = 10, by = 1)
grid <- expand.grid(x1,x2)

set.seed(5474832)
sigma2 <- 5 # variance 5
phi <- 1 # some correlation length
sample_data <- grf(grid = grid,
                   cov.pars = c(sigma2, phi),
                   cov.model = "matern",
                   kappa = 1.5)
lim <- 8 # setting up min/max for consistent plotting
image(sample_data, col = viridis(100), zlim = c(-lim, lim))

## increase numbers 
x1 <- seq(from = 0, to = 10, by = 0.5)
x2 <- seq(from = 0, to = 10, by = 0.5)
grid <- expand.grid(x1,x2)
sample_data <- grf(grid = grid,
cov.pars = c(sigma2, phi),
cov.model = "matern",
kappa = 1.5)
image(sample_data, col = viridis(100), zlim = c(-lim,lim))



#### automatically generate multiple samples 
# from our model at the same time
sample_data100 <- grf(grid = grid,
                      cov.pars = c(sigma2, phi),
                      cov.model = "matern",
                      kappa = 1.5,
                      nsim = 100)
par(mfrow=c(2,2), mar = c(4,2,2,2))
image(sample_data100, sim.number = 1, col = viridis(100),
      zlim = c(-lim,lim), main = 'Sample 1')
image(sample_data100, sim.number = 2, col = viridis(100),
      zlim = c(-lim,lim), main = 'Sample 2')
image(sample_data100, sim.number = 3, col = viridis(100),
      zlim = c(-lim,lim), main = 'Sample 3')
image(sample_data100, sim.number = 100, col = viridis(100),
      zlim = c(-lim,lim), main = 'Sample 100')


#### Spatial modelling
data(parana)
par(mar=c(4,2,2,2))
plot(parana)

### Calculating and plotting the sample variogram for the parana dataset
sample_vario <- variog(parana, option='bin')
par(mar=c(4,4,2,2))
plot(sample_vario, pch = 19)
sample_vario <- variog(parana, option='bin', max.dist=400)
vari.default <- variofit(sample_vario)
vari.default

## plot
par(mar=c(4,4,2,2))
plot(sample_vario, pch = 19)
lines(vari.default)

model_ml <- likfit(parana, ini.cov.pars = c(10,1))
model_ml
summary(model_ml)

model_trend <- likfit(parana, trend = '2nd', ini.cov.pars = c(10,1), 
                      cov.model = 'matern', kappa = 5/2)
model_trend
summary(model_trend)


### Validation of likfit
xv.ml <- xvalid(parana, model = vari.default)
par(mfrow=c(3,2),mar=c(4,2,2,2))
plot(xv.ml, error = TRUE, std.error = FALSE, pch = 19)

## 
xv.ml <- xvalid(parana, model = model_trend)
plot(xv.ml, error = TRUE, std.error = FALSE, pch = 19)


##
model_bad <- likfit(parana, fix.nugget = TRUE, ini.cov.pars = c(10,1), 
                    cov.model = 'matern',
                    kappa = 3/2)
xv.ml <- xvalid(parana, model = model_bad)
par(mfrow=c(3,2),mar=c(4,2,2,2))
plot(xv.ml, error = TRUE, std.error = FALSE, pch = 19)

## Prediction
pred_grid <- expand.grid(seq(100, 800, by = 5), seq(0, 550, by = 5))
# obj.model is the model we want to use for prediction
preds <- krige.conv(parana, loc=pred_grid,
                    krige=krige.control(obj.model=model_ml))
image(preds, col = viridis::viridis(100), zlim = c(0,max(c(preds$predict))),
      coords.data = parana[1]$coords, main = 'Mean', xlab = 'x', ylab = 'y',
      x.leg = c(700, 900), y.leg = c(20, 70))
image(preds, values = preds$krige.var, col = heat.colors(100)[100:1],
      zlim = c(0,max(c(preds$krige.var))), coords.data = parana[1]$coords,
      main = 'Variance', xlab = 'x', ylab = 'y', x.leg = c(700, 900), 
      y.leg = c(20, 70))


###############################################################################
Series1 <- ts(read.table(file="data/Series1.csv",header=FALSE))
Series2 <- ts(read.table(file="data/Series2.csv",header=FALSE))
Series3 <- ts(read.table(file="data/Series3.csv",header=FALSE))
Series4 <- ts(read.table(file="data/Series4.csv",header=FALSE))