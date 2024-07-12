##Sample Mean vs Theoretical Mean

#Show the sample mean and compare it to the theoretical mean of the distribution

#Provided data for this exercise
lambda<-0.2
#expected mean
miu<-1/lambda # expected mean
sigma<-1/lambda # expected std dev
n<-40 # number of observations
nsim<-1000 # number of simulations

set.seed(1000)
exp_means=NULL
for (i in 1 : nsim) exp_means = c(exp_means, mean(rexp(n,lambda)))
sample_mean <- mean(exp_means)
hist(exp_means, main="Histogram of 1000 exponential distribution means",
     breaks=25)
abline(v = miu, col= 2, lwd = 2,lty=2)
abline(v = sample_mean, col= 3, lwd = 1)
legend('topright', c("Expected Mean", "Sample Mean"),
       lty= c(2,1),
       bty = "n", col = c(col = 2, col = 3))

#Expected Mean
miu

#Sample Mean
sample_mean



##Sample variance vs Theoretical Variance

#The Theoratical variance is equal to the variance of the original population 
#divided by the number of samples n

#Expected Variance

lambda<-0.2

sigma <- 1/lambda # standard dev
expected_var <- sigma^2/n # expected variance
expected_var

#Sample Variance
sample_var <- var(exp_means) # sample variance
sample_var

hist(exp_means,main="Exponential distribution for 1000 means",
     breaks=20,xlim=c(0,9))
abline(v = expected_var, col= 2, lwd = 2,lty=2)
abline(v = sample_var, col= 3, lwd = 1)



#Verify the results by comparing histograms with analytical results and perform
#tests for distribution type
lambda <- 0.2
miu <- 1/lambda
sigma <- 1/lambda
n <-40
nsim=10000
for (i in 1 : nsim) exp_means = c(exp_means, mean(rexp(n,lambda)))
sample_mean <- mean(exp_means)

hist(exp_means, breaks = 50, prob = TRUE, 
     main = "Exponential Distrib Mean compared to Normal Distrib")
xfit <- seq(min(exp_means), max(exp_means), length = 100)
yfit <- dnorm(xfit, mean = miu, sd = (sigma/sqrt(40)))
lines(xfit, yfit, pch = 22, col = "blue", lwd = 4)

qqnorm(exp_means);qqline(exp_means)
legend('topright', c("Expected Variance", "Sample Variance"), lty= c(2,1),
       bty = "n", col = c(col = 2, col = 3))
