library("rstan")
library("ggplot2")
library("data.table")
library("RColorBrewer")

# set the parameters
dat <- list(
  priceMean = c(3000,12000),
  priceSD = c(500,3000),
  totPriorMean = 35000,
  totPriorSD = 7500
)

# call stan model
fit <- stan(file = 'strangeLikelihoodExperiment/stanModel.stan',data = dat,
            iter = 30000, chains = 4)

smp <- extract(fit)

# plot the posterior vs prior for tot Price
ggplot() + geom_density(aes(x = smp$totPrice)) + 
  geom_line(aes(x = seq(from = 5000, to = 60000, by = 1),
                dnorm(seq(from = 5000, to = 60000, by = 1),
                      mean = dat$totPriorMean, sd =  dat$totPriorSD)), col = 'red') + 
  geom_vline(aes(xintercept = mean(smp$totPrice))) + geom_vline(aes(xintercept =dat$totPriorMean),col = 'red')


# loss function 
lossFunction <- function(theta_hat,theta,risk = 80000)
{
  val <- 0
  if (theta_hat > theta){
    val <- risk
  } else if (abs(theta_hat-theta) <= 250 ){
    val <- -2*abs(theta)
  }
  else {
    val <- abs(theta - theta_hat - 250)
  }
   return(val)
}


# function calling the loss function and returning the expected loss (with respect to the posterior)
calcLossForRisk <- function(theta_hat_range, posteriorSamples, risk = 80000)
loss <- sapply(theta_hat_range,
       function(theta_hat){
         return(mean(sapply(posteriorSamples,function(postSmp){
            return(lossFunction(theta_hat,postSmp,risk))   
         }))
        )
        }
)

# range of values to estimate the expected loss
theta_hat_range <- seq(1000,40000,100)

# evaluate loss for various  values of risk parameter
loss80000 <- calcLossForRisk(theta_hat_range,smp$totPrice,80000)
loss30000 <- calcLossForRisk(theta_hat_range,smp$totPrice,30000)
loss120000 <- calcLossForRisk(theta_hat_range,smp$totPrice,120000)

# process and visualise
lossDat <- data.table(theta = theta_hat_range, 
                      loss80000 = loss80000,
                      loss30000 = loss30000,
                      loss120000 = loss120000)

lossDatMelt <- melt.data.table(lossDat, id.vars = 'theta',
                               variable.name = 'risk', value.name = 'loss')

minPointsDat <- lossDatMelt[,.SD[which.min(loss)], by = risk]

ggplot(lossDatMelt) + geom_line(aes(x=theta, y=loss, col = risk)) + 
  geom_vline(data = minPointsDat,aes(xintercept = theta, col = risk),show.legend = F) + 
  scale_colour_manual( breaks = minPointsDat[order(theta),risk], values = brewer.pal(3,"Set1") )
