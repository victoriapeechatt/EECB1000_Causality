##### Statistical Rethinking ##### 

##### The Code 

##### Designing a Bayesian estimator: The Basics 
##### The system here is to estimate the proportion of earth covered in water. 
##### We're going to pick some spots randomly across the globe 
##### W for water 
##### L for land 
##### and list some possibilities for the proportion of earth covered in water 
##### using the sample determine the probabilities that each of our hypothetical 
##### globes is the globe that gave us this sample 

##### Simulate a sample #####

sample = c("W", "L", "W", "W", "W", "L", "W", "l", "W")

W = sum(sample=="W")
L = sum(sample=="L")

p = c(0, 0.25, 0.5, 0.75, 1) #our hypothetical proportions of water on the globe 

garden_of_data = data.frame(
  p, 
  W1 = c("0", "1", "2","3","4"), 
  L2 = c("0", "3", "4", "3", "0"), 
  W3 = c("0", "3", "8", "9", "0"),
  W4 = c("0", "3", "16", "27", "0"),
  W5 = c("0", "3", "32", "81", "0"),
  L6 = c("0", "9", "64", "81", "0"),
  W7 = c("0", "9", "128", "243", "0"),
  L8 = c("0", "27", "256", "243", "0"),
  W9 = c("0", "27", "512", "729", "0")
)
ways = sapply(p, function(q) (q*4)^W * ((1-q)*4)^L )
prob = ways/sum(ways)

cbind(p, ways, prob)

barplot(prob, names.arg = p, 
        xlab = "Proportion of Water",
        ylab = "Probability of Sample")

##### Simulating samples 

simulate_globe = function( p=0.7, N=9 ){
  
  sample(c("W","L"), size = N, prob = c(p, 1-p), replace = TRUE)
  
}
simulate_globe()
replicate(simulate_globe(), n=10)

##### Testing simulation 

## Extremes

simulate_globe(p=1, N=10)

## Proportions for large samples should be the same as the value you set 

sum(simulate_globe(p = 0.67, N=1e4) == "W") / 1e4

##### Code the estimator as a function 

compute_posterior = function(the_sample, possibilities = c(0,0.25,0.5,0.75,1)){
  
  W = sum(the_sample=="W")
  L = sum(the_sample=="L")
  ways = sapply(possibilities, function(q) (q*4)^W * ((1-q)*4)^L )
  prob = ways/sum(ways)
  data.frame( possibilities, ways, prob = round(prob,3) )
}

compute_posterior(simulate_globe() )

post_samples = rbeta(1000, 6+1, 3+1)

devtools::install_github("stan-dev/cmdstanr")
devtools::install_github("rmcelreath/rethinking")
library(rethinking)

dens( post_samples, lwd=4, col=2, xlab="proportion water", adj = 0.1)
curve( dbeta(x,6+1,3+1), add = TRUE, lty=2, lwd=3)

# simulate posterior predictive distribution 

post_samples = rbeta(1000, 6+1, 3+1)
predictive_posterior = sapply(post_samples, function(p) sum(simulate_globe(p,10)=="W") )
table_pred_post = table(predictive_posterior)
plot(table_pred_post)
for ( i in 0:10 ) lines(c(i,i),c(0,table_pred_post[i+1]), lwd=4, col=4)

##### Bayesian linear regression ##### 
##### Two continuous variables 

# Simulate the data 

sim_weight = function(H, b, sd ){
  a = rnorm(length(H),0,sd)
  W = b*H + a
  return(W)
}

H = runif(1000, 130,170)
W = sim_weight(H, b=0.5, sd = 5)

# Enter a list of deterministic and distributional assumptions
# finds the quadratic approximation of the posterior distribution 

m <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + b*H, 
    a ~ dnorm(0,10), 
    b ~ dunif(0,1),
    sigma ~ dunif(0,10)
  ), data = list(W=W, H=H) )

precis(m)
pairs(m)

# Extract samples from the model 
# Plot samples of data and 
# lines from the posterior 

post = extract.samples(m)
plot(H,W, col = 2, lwd = 3, 
     xlab = "height", ylab = "weight")
for (j in 1:length(H))
  abline( a=post$a[j], b = post$b[j], lwd = 1)

##### Bayesian model with categorocal data #####

##### Sex -> Height -> Weight <- Sex  

# simulate data 

#S=1 female; S=2 male 

sim_HW = function(S,b,a){
  N = length(S)
  H = ifelse(S==1,150,160) + rnorm(N, 0,5)
  W = a[S] + b[S]*H + rnorm(N, 0,5)
  data.frame(S,H,W)
}

S = rbern(100) + 1 #to make sex index of 1 and 2 

dat = sim_HW(S,b=c(0.5,0.6),a=c(0,0))

plot(dat)
