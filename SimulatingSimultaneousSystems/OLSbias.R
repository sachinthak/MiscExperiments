library(mvtnorm)
library(data.table)
library(ggplot2)

# set ground truth 
#(to simulate simultaneous equations example in Chap 14 of Using Econometrics - 6th Ed by
# Studenmund)
beta1 <- -1
beta2 <- 1
alpha1 <- 1
alpha2 <- 1

# noise parameters
sig2S  <- 3
sig2D <- 2
sig2X <- 1
sig2Z <- 4
uX <- 3
uZ <- 5
prosqr <- 0.4 

# Number of  observations used to fit the regression models
N <- 50

# Number of simulation runs to get the distribution of estimates
Nsim <- 5000





# pre allocate memory
beta1SmpOLS <- array(dim = Nsim )
beta2SmpOLS <- array(dim = Nsim )
alpha1SmpOLS <- array(dim = Nsim )
alpha2SmpOLS <- array(dim = Nsim )
beta1IV <- array(dim = Nsim )
beta2IV <- array(dim = Nsim )
alpha1IV <- array(dim = Nsim )
alpha2IV <- array(dim = Nsim )


covsqrXZ <- prosqr*sig2X*sig2Z
cov <- matrix(data = c(sig2X,sqrt(covsqrXZ), sqrt(covsqrXZ), sig2Z), nrow = 2 )

for (j in 1:Nsim) {
  
  XZsmp <- rmvnorm(n = N, mean = c(uX,uZ),sigma = cov)
  Xsmp <- XZsmp[,1]
  Zsmp <- XZsmp[,2]
  epsDemand <- rnorm(N,0,sqrt(sig2D))
  epsSupply <-rnorm(N,0,sqrt(sig2S))
  
  Psmp <- 1/(beta1-alpha1)*(alpha2*Zsmp - beta2*Xsmp + epsSupply - epsDemand)
  Qsmp <- beta1*Psmp +beta2*Xsmp + epsDemand
  
  
  
  dat <- data.table(Q = Qsmp, X = Xsmp, Z = Zsmp, P = Psmp)
  
  fitDemandOLS <- lm(data = dat, formula = 'Q ~ 1 + P + X')
  fitSupplyOLS <- lm(data = dat, formula = 'Q ~ 1 + P + Z')
  fitFirstStage <- lm(data = dat, formula = 'P ~ 1 + X + Z')
  dat$PHat <- predict(fitFirstStage,dat)
  
  fitDemandSecondStage <- lm(data = dat, formula = 'Q ~ 1 + PHat + X')
  fitSupplySecondStage <- lm(data = dat, formula = 'Q ~ 1 + PHat + Z')
  
  
  beta1SmpOLS[j] <- as.numeric(coef(fitDemandOLS)['P'])
  beta2SmpOLS[j] <- as.numeric(coef(fitDemandOLS)['X'])
  alpha1SmpOLS[j] <- as.numeric(coef(fitSupplyOLS)['P'])
  alpha2SmpOLS[j] <- as.numeric(coef(fitSupplyOLS)['Z'])
  beta1IV[j] <- as.numeric(coef(fitDemandSecondStage)['PHat'])
  beta2IV[j] <- as.numeric(coef(fitDemandSecondStage)['X'])
  alpha1IV[j] <- as.numeric(coef(fitSupplySecondStage)['PHat'])
  alpha2IV[j] <- as.numeric(coef(fitSupplySecondStage)['Z'])
  
}

simDat <- data.table( beta1SmpOLS = beta1SmpOLS,
                      beta2SmpOLS = beta2SmpOLS,
                      alpha1SmpOLS = alpha1SmpOLS,
                      alpha2SmpOLS = alpha2SmpOLS,
                      beta1IV = beta1IV,
                      beta2IV = beta2IV,
                      alpha1IV = alpha1IV,
                      alpha2IV = alpha2IV )


meltSimDat <- melt.data.table(simDat, measure.vars = c('beta1SmpOLS','beta2SmpOLS','alpha1SmpOLS','alpha2SmpOLS',
                                                       'beta1IV','beta2IV','alpha1IV','alpha2IV'), value.name = 'prediction')


ggplot(meltSimDat[variable %in% c('beta1SmpOLS','beta1IV')]) + geom_density(aes(x=prediction, col = variable)) + scale_x_continuous(limits = c(-5,5)) + geom_vline(aes(xintercept=beta1))
ggplot(meltSimDat[variable %in% c('alpha1SmpOLS','alpha1IV')]) + geom_density(aes(x=prediction, col = variable)) + scale_x_continuous(limits = c(-5,5)) + geom_vline(aes(xintercept=alpha1))
ggplot(meltSimDat[variable %in% c('beta2SmpOLS','beta2IV')]) + geom_density(aes(x=prediction, col = variable)) + scale_x_continuous(limits = c(-2.5,2.5)) + geom_vline(aes(xintercept=beta2))
ggplot(meltSimDat[variable %in% c('alpha2SmpOLS','alpha2IV')]) + geom_density(aes(x=prediction, col = variable)) + scale_x_continuous(limits = c(-2.5,2.5)) + geom_vline(aes(xintercept=alpha2))

