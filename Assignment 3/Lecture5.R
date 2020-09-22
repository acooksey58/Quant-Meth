require(mosaic)

############################################
#Distributions Expected value and variance
#Standard normal, then shifted to the left
x <- seq(-6,6,length=500)
plot(x,dnorm(x,mean=0,sd=1),type = "l",lty=1,lwd=3,col="blue",main="Normal Distribution",ylim=c(0,0.5),xlim=c(-6,6),ylab="Density")
curve(dnorm(x,-1,1),add=TRUE,lty=2,col="blue")
curve(dnorm(x,-2,1),add=TRUE,lty=3,col="blue")
legend(2,.5,legend=c("N ~ (0, 1)","N ~ (-1, 1)","N ~ (-2, 1)"),lty=1:3,col="blue")

#Standard normal, then shifted to the right
x <- seq(-6,6,length=500)
plot(x,dnorm(x,mean=0,sd=1),type = "l",lty=1,lwd=3,col="purple",main="Normal Distribution",ylim=c(0,0.5),xlim=c(-6,6),ylab="Density")
curve(dnorm(x,1,1),add=TRUE,lty=2,col="purple")
curve(dnorm(x,2,1),add=TRUE,lty=3,col="purple")
legend(-5.5,.5,legend=c("N ~ (0, 1)","N ~ (1, 1)","N ~ (2, 1)"),lty=1:3,col="purple")

#Standard normal, then increased variance
x <- seq(-6,6,length=500)
plot(x,dnorm(x,mean=0,sd=1),type = "l",lty=1,lwd=3,col="black",main="Normal Distribution",ylim=c(0,0.5),xlim=c(-6,6),ylab="Density")
curve(dnorm(x,0,1.5),add=TRUE,lty=2,col="red")
curve(dnorm(x,0,2),add=TRUE,lty=3,col="black")
legend(-5.5,.5,legend=c("N ~ (0, 1)","N ~ (0, 2.25)","N ~ (0, 4)"),lty=1:3,col=c("black","red","black"))



##################################
#Simulation using data from a known DGP, rnorm
# set a seed
set.seed(123456)
library(dplyr)
library(ggplot2)
# draw a random sample of 10 from the normal
x <- rnorm(10, mean = 5, sd = 1)
hist(x)
mean(x)
# resample with replacement
resample <- sample(x, replace = TRUE)
mean(resample)
# Resample 1000 times, and find the mean of each
data_frame(num = 1:1000) %>% 
  group_by(num) %>% 
  mutate(means = mean(sample(x, replace = TRUE))) %>% 
  ggplot(aes(x = means)) +
  geom_freqpoly()

# Resample a 5000 times, and find the mean of each
data_frame(num = 1:5000) %>% 
  group_by(num) %>% 
  mutate(means = mean(sample(x, replace = TRUE))) %>% 
  ggplot(aes(x = means)) +
  geom_freqpoly()
#################################################


#Central Limit Theorem
DieOutcome <- sample(1:6,10000, replace= TRUE)
hist(DieOutcome, col ="light blue")
abline(v=3.5, col = "red",lty=1)

x10 <- c()
k =10000
for ( i in 1:k) {
  x10[i] = mean(sample(1:6,10, replace = TRUE))}
hist(x10, col ="pink", main="Sample size =10",xlab ="Outcome of die roll")
abline(v = mean(x10), col = "Red")
abline(v = 3.5, col = "blue")

x30 <- c()
x100 <- c()
x1000 <- c()
k =10000
for ( i in 1:k){
  x30[i] = mean(sample(1:6,30, replace = TRUE))
  x100[i] = mean(sample(1:6,100, replace = TRUE))
  x1000[i] = mean(sample(1:6,1000, replace = TRUE))
}
par(mfrow=c(1,3))
hist(x30, col ="green",main="n=30",xlab ="die roll")
abline(v = mean(x30), col = "blue")

hist(x100, col ="light blue", main="n=100",xlab ="die roll")
abline(v = mean(x100), col = "red")

hist(x1000, col ="orange",main="n=1000",xlab ="die roll")
abline(v = mean(x1000), col = "red")

x <- c()
k =10000  
for ( i in 1:k) {  
  x[i] = mean(sample(0:1,100, replace = TRUE))}  
hist(x, col ="light green", main="Sample size = 100",xlab ="flipping coin ")  
abline(v = mean(x), col = "red")

###############################################

#Simulation using real populations
# upload data that is the whole population of that race
population <- TenMileRace
#count
nrow(population)
#True mean age of the population that we are trying to estimate
mean(population$age)
#take a sample
planet.sample <- sample(population, 100)
#calculate mean of age
with(planet.sample, mean(age))
#calculate sd of age
with(planet.sample, sd(age))
#calculate prop of Females
with(planet.sample, mean(sex=="F"))
#take a new sample and calculate mean sd and gender
with(sample(population,100), mean(age))
with(sample(population,100), sd(age))
with(sample(population,100), mean(sex=="F"))

#simulate 500 
sample.means <- do(500) * with(sample(population,100), mean(age))
sample.sds <- do(500) * with(sample(population,100), sd(age))
sample.props <- do(500) * with(sample(population,100), mean(sex=="F"))
#dataframe it
df.sample.means <- data.frame(matrix(unlist(sample.means), nrow=length(sample.means), byrow=T))
df.sample.sds <- data.frame(matrix(unlist(sample.sds), nrow=length(sample.sds), byrow=T))
df.sample.props <- data.frame(matrix(unlist(sample.props), nrow=length(sample.props), byrow=T))
# calculate the sds, the mean and histogram it
sd(df.sample.means) # standard error for mean age
sd(df.sample.sds) # standard error for sd of age
sd(df.sample.props) # standard error for fraction female
mean(as.numeric(df.sample.means))
hist(as.numeric(df.sample.means))

#now resample from a sample
planet.sample = sample(population, 100) # one sample from the population
with(resample(planet.sample,100), mean(age))
with(resample(planet.sample,100), mean(age))
with(resample(planet.sample,100), mean(age))
# now simulate samples 500 times each
resample.means <- do(500) * with(resample(planet.sample,100), mean(age))
resample.sds <- do(500) * with(resample(planet.sample,100), sd(age))
resample.props <- do(500) * with(resample(planet.sample,100), mean(sex=="F"))
# dataframe it
df.resample.means <- data.frame(matrix(unlist(resample.means), nrow=length(resample.means), byrow=T))
df.resample.sds <- data.frame(matrix(unlist(resample.sds), nrow=length(resample.sds), byrow=T))
df.resample.props <- data.frame(matrix(unlist(resample.props), nrow=length(resample.props), byrow=T))
# calculate the sds, the mean and histogram it
sd(df.resample.means) # standard deviation of mean ages
sd(df.resample.sds) # standard deviation of sd of age
sd(df.resample.props) # standard deviation of fraction female
mean(as.numeric(df.resample.means))
hist(as.numeric(df.resample.means))


read.csv(mosaic)

read.csv(dhs_ipv.csv)
