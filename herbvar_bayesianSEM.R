
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
pkgTest("asbio")
pkgTest("blavaan")
pkgTest("chisq.posthoc.test")
pkgTest("data.table")
pkgTest("emmeans")
pkgTest("forcats")
pkgTest("foreign")
pkgTest("inspectdf")
pkgTest("knitr")
pkgTest("gridExtra")
pkgTest("ggpubr")
pkgTest("ggpmisc")
pkgTest("lattice")
pkgTest("lavaan")
pkgTest("lavaanPlot")
pkgTest("lme4")
pkgTest("lmtest")
pkgTest("magrittr")
pkgTest("MASS")
pkgTest("Matrix")
pkgTest("mice")
pkgTest("nlme")
pkgTest("plot3D")
pkgTest("psych")
pkgTest("rstan")
pkgTest("semPlot")
pkgTest("shinystan")
pkgTest("sjPlot")
pkgTest("tidyverse")
pkgTest("tinytable")
pkgTest("vegan")

options(mc.cores = parallel::detectCores())


##### MODE FUNCTION ###
########################

mode_calc = function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

#######################3 
##########HERBVAR DATA############################
######################################3

herbvar = read_csv(file = "Documents/GitHub/EECB1000_Causality/pivoted.csv")
### Two notes: 
### I used the  plantHeight_cm column for plant height,
### but sizeCentscale seemed to be the most complete 
### so i brought that in as "plant size" in case we want to use that
### When I made the new csv, I got rid of all "leafPerc" columns with NA's 
### but kept all 0's 
hist(herbvar$leaf_percent)
hist(subset(herbvar, leaf_percent >0)$leaf_percent)

herbvar_q = 
  herbvar %>%
  mutate(Lat_abs = abs(Lat)) %>%
  group_by(plant_id, biome, Lat_abs, plant_height, plant_size) %>%
  summarise(first_q = quantile(x = leaf_percent, probs = 0.25),
            median = quantile(x = leaf_percent, probs = 0.5), 
            third_q = quantile(x = leaf_percent, probs = 0.75), 
            IQR = third_q-first_q, 
            mean = mean(leaf_percent),
            std = sqrt(var(leaf_percent)), 
            vari = var(leaf_percent),
            mode = mode_calc(leaf_percent),
            skew = skew(leaf_percent), 
            kurt = kurt(leaf_percent),
            cv = std/mean,
            MAD_median = mad(leaf_percent, center = median(leaf_percent)),
            MAD_mean = mad(leaf_percent, center = mean(leaf_percent), na.rm = TRUE),
            sarle_bimo = (skew(leaf_percent)^2 + 1)/
                          (kurt(leaf_percent)+
                             ((3*(length(leaf_percent)-1)^2)/
                                ((length(leaf_percent)-2)*(length(leaf_percent)-3))
                              )
                           ),
            alpha_beta = ((mean^2 - mean^3 - mean*std^2)/vari), 
            beta_beta = ((mean - 2*mean^2 + mean^3 - vari + mean*vari)/vari),
            alpha_gamma = ((mean^2)/(vari^2)),
            beta_gamma = mean/(vari^2)
            )

######INITUAL VISUALS########

hist((herbvar_q$mean))# --> consider zero-inflated models, lognormal dist 
hist(log(herbvar_q$MAD_mean))
hist(log(herbvar_q$median))
hist((herbvar_q$mode)) 
hist(log(herbvar_q$vari))
hist((herbvar_q$skew)) 
hist((herbvar_q$kurt)) # --> consider bimodal dist, or heavily right skewed 

# Gamma dist with moments matched
plot(0, 0, xlim = c(0, 10), ylim = c(0, 10), type = "n")
shape = herbvar_q$alpha_gamma
rate = herbvar_q$beta_gamma  
for(i in seq_along(shape)){
  curve(dgamma(x, shape = shape[i], rate = rate[i]), from = 0, to = 10, col = i, add = TRUE)
}

# Beta dist with moments matched
plot(0, 0, xlim = c(0, 1), ylim = c(0, 5), type = "n")
shape1 = herbvar_q$alpha_beta
shape2 = herbvar_q$beta_beta  
for(i in seq_along(shape1)){
  curve(dbeta(x, shape1 = shape1[i], shape2 = shape2[i]), from = 0, to = 10, col = i, add = TRUE)
}

### Plant height predicting mean ####

ggplot(data = herbvar_q, aes(x = plant_height)) +
  geom_point(aes(x = plant_height, 
                 y = log(mean),
                 color = biome, 
                 group = biome)) +
  xlab('Plant height') +
  ylab('mean (log)') +
  theme_grey(base_size = 16)

### Plant size (standardized) predicting mean ####

ggplot(data = herbvar_q, aes(x = plant_size)) +
  geom_point(aes(x = plant_size, 
                 y = mean,
                 color = biome, 
                 group = biome)) +
  xlab('Plant size (standardized)') +
  ylab('mean') +
  theme_grey(base_size = 16)

# seems wrong, filtering out Lat == NA 

herbvar_q %>% filter(!is.na(Lat_abs)) %>%
  ggplot(aes(x = plant_size)) +
  geom_point(aes(x = plant_size, 
                 y = log(mean),
                 color = biome, 
                 group = biome)) +
  xlab('Plant size (standardized)') +
  ylab('mean (log)') +
  theme_grey(base_size = 16)

### Plant height predicting median ####

ggplot(data = herbvar_q,aes(x = plant_height)) +
  geom_point(aes(x = log(plant_height), 
                 y = log(median),
                 color = biome, 
                 group = biome)) +
  xlab('Plant height (log)') +
  ylab('Median (log)') +
  theme_grey(base_size = 16)

### Plant height predicting MAD_mean ####

ggplot(data = herbvar_q, aes(x = plant_height)) +
  geom_point(aes(x = plant_height, 
                 y = MAD_mean,
                 color = biome, 
                 group = biome)) +
  xlab('Plant height') +
  ylab('Mean absolute difference') +
  facet_wrap (. ~ biome, scales = 'free', shrink = T)+
  theme_grey(base_size = 16)

### Plant height  predicting skew  (facet) ####

ggplot(data = herbvar_q, aes(x = plant_height))+
  geom_point(aes(x = plant_height, 
                  y = skew,
                  color = biome, 
                  group = biome)) +
  xlab('Plant height') +
  ylab('skew') +
  facet_wrap (. ~ biome, scales = 'free', shrink = T)+
  theme_grey(base_size = 16)

### Plant height predicting MAD_median  (facet) ####

ggplot(data = herbvar_q, aes(x = plant_height)) +
  geom_point(aes(x = log(plant_height), 
                  y = MAD_median,
                  color = biome, 
                  group = biome)) +
  xlab('Plant height (log)') +
  ylab('mediann absolute difference') +
  facet_wrap (. ~ biome, scales = 'free', shrink = T)+
  theme_grey(base_size = 16)

### Latitude (absolute) predicting mean ####

ggplot(data = herbvar_q, aes(x = Lat_abs)) +
  geom_point(aes(x = Lat_abs, 
                 y = log(mean),
                 color = biome, 
                 group = biome)) +
  xlab('Latitude') +
  ylab('Mean (log)') +
  theme_grey(base_size = 16)+
  stat_smooth(aes(x = Lat_abs, y=log(mean)), 
              method = "glm",
              formula = y ~ poly(x,2), 
              se = TRUE, fullrange = TRUE,
              size=2)

### Latitude (absolute) predicting median ####

ggplot(data = herbvar_q, aes(x = Lat_abs)) +
  geom_point(aes(x = Lat_abs, 
                 y = log(median),
                 color = biome, 
                 group = biome)) +
  xlab('Latitude') +
  ylab('Median (log)') +
  theme_grey(base_size = 16)+
  stat_smooth(aes(x = Lat_abs, y=log(median)), 
              method = "glm",
              formula = y ~ poly(x,2), 
              se = TRUE, fullrange = TRUE,
              size=2)

### Latitude (absolute) predicting skew ####

ggplot(data = herbvar_q, aes(x = Lat_abs)) +
  geom_point(aes(x = Lat_abs, 
                 y = skew,
                 color = biome, 
                 group = biome)) +
  xlab('Latitude') +
  ylab('skew') +
  theme_grey(base_size = 16)+
  stat_smooth(aes(x = Lat_abs, y=skew), 
              method = "glm",
              formula = y ~ poly(x,2), 
              se = TRUE, fullrange = TRUE,
              size=2) 

### Latitude (absolute) predicting MAD_mean ####

ggplot(data = herbvar_q, aes(x = Lat_abs)) +
  geom_point(aes(x = Lat_abs, 
                  y = MAD_mean,
                  color = biome, 
                  group = biome)) +
  xlab('Latitude') +
  ylab('Mean absolute difference') +
  theme_grey(base_size = 16) + 
  stat_smooth(aes(x = Lat_abs, y=MAD_mean), 
              method = "glm",
              formula = y ~ poly(x,2), 
              se = TRUE, fullrange = TRUE,
              size=2) 

### Latitude (absolute) predicting log(var) ####

ggplot(data = herbvar_q, aes(x = Lat_abs)) +
  geom_point(aes(x = Lat_abs, 
                 y = log(vari),
                 color = biome, 
                 group = biome)) +
  xlab('Latitude') +
  ylab('Variance (log)') +
  theme_grey(base_size = 16)+
  stat_smooth(aes(x = Lat_abs, y=log(vari)), 
              method = "glm",
              formula = y ~ poly(x,2), 
              se = TRUE, fullrange = TRUE,
              size=2) 

### Latitude (absolute) predicting mean  (facet) ####

ggplot(data = herbvar_q, aes(x = Lat_abs)) +
  geom_point(aes(x = Lat_abs, 
                  y = mean,
                  color = biome, 
                  group = biome)) +
  xlab('Latitude') +
  ylab('mean') +
  facet_wrap (. ~ biome, scales = 'free', shrink = T)+
  theme_grey(base_size = 16)

### Latitude (absolute) predicting skew  (facet) ####

ggplot(data = herbvar_q, aes(x = Lat_abs)) +
  geom_point(aes(x = Lat_abs, 
                  y = skew,
                  color = biome, 
                  group = biome)) +
  xlab('Latitude') +
  ylab('skew') +
  facet_wrap (. ~ biome, scales = 'free', shrink = T)+
  theme_grey(base_size = 16)

### Frequencies of modes  ####

herbvar_q %>%
  #filter(mode!=0)%>%
  ggplot(aes(x = mode, fill = biome)) +
  geom_histogram(binwidth = 10, color = "black") +
  xlab('mode') +   
  ylab('Count') +   
  theme_grey(base_size = 16)

### 3D plots ####

p  = cloud(MAD_mean ~ plant_size*Lat_abs, pch=".", data = herbvar_q)
s = xyplot(Lat_abs ~ plant_size, pch=".", aspect = 2.44, data = herbvar_q)
print(s, split = c(1, 1, 2, 1), more = TRUE)
print(p, split = c(2, 1, 2, 1))

scatter3D(herbvar_q$Lat_abs, herbvar_q$plant_size, herbvar_q$mean,
          clab = c("MAD","mean"),
          theta = 60, phi = 5,
          pch = 18, bty = "u", colkey = TRUE, ticktype = "detailed",
          main ="Lat_abs x Plant size x MAD", col.panel ="steelblue", expand =0.4, 
          col.grid = "darkblue")

scatter3D(herbvar_q$Lat_abs, herbvar_q$plant_size, herbvar_q$MAD_mean, 
          theta = 15, phi = 90)


###### IMPUTATION      ###########

herbvar_f = herbvar_q %>% filter(!is.na(Lat_abs))
# Shows how much of each column is missing 
data_missing <- unlist(lapply(herbvar_f, function(x) sum(is.na(x))))/nrow(herbvar_f)
sort(data_missing[data_missing > 0], decreasing = TRUE)

# mice mice baby 
imputation1 <- mice(herbvar_f,method="pmm", maxit=5)
meth = imputation1$method

# Shows what variables are being used to impute others 
# You can change the predictors by changing the pred object 
imputation1$predictorMatrix

# The imputed data:
head(imputation1$data$skew)

# Reincorporating it into the rest of the data
herbvar_f = complete(imputation1, 1)

######### FACTOR ANALYSIS ########

vars = as.matrix(herbvar_f[,c(6,7,8,10,12,13,14,15)])
DH.test(herbvar_f[,c(5:15)], Y.names = NULL)

vars <- decostand(vars, method = "standardize")

factor = factanal(vars, factors = 3, rotation ="promax", scores = "regression")
load  = factor$loadings
load
fa.diagram(load)
plot(load, type = "n")
text(load, labels = names(herbvar_f[,c(6,7,8,10,12,13,14,15)]), cex=0.7)

q1_median_mode = factor$scores[,1]
variance_q3_mean = factor$scores[,2]
skew_kurt = factor$scores[,3]

herbvar_f = cbind(herbvar_f, q1_median_mode)
herbvar_f = cbind(herbvar_f, variance_q3_mean)
herbvar_f = cbind(herbvar_f, skew_kurt)

# PLOT 
data = gather(herbvar_f, variable, value, q1_median_mode:skew_kurt, factor_key = TRUE)

ggplot(data, aes(x = Lat_abs, y = value)) + 
  geom_violin(aes(fill = Lat_abs))+
  #geom_boxplot(outlier.shape = NA)+
  facet_wrap(.  ~ variable, scales = "fixed", shrink = TRUE)+
  xlab("")+
  ylab("")

####### MODELS ###############

model1 = glm(q1_median_mode ~ Lat_abs + plant_size, family = "gaussian", data = herbvar_f)
summary(model1)

model2 = glm(variance_q3_mean ~ Lat_abs + plant_size, family = "gaussian", data = herbvar_f)
summary(model2)

model3 = glm(skew_kurt ~ Lat_abs + plant_size, family = "gaussian", data = herbvar_f)
summary(model3)

model4 = glm(skew_kurt + variance_q3_mean + q1_median_mode ~ Lat_abs + plant_size, family = "gaussian", data = herbvar_f)
summary(model4)

herbvar_scaled = apply(herbvar_f[,c(3:26)], MARGIN = 2, scale)

##########SEM MODELS ###############

### SEM Model 1: using factors made with FA #########

sem_mod =   '
  # observed outcomes to observed predictors
    q1_median_mode + variance_q3_mean + skew_kurt ~ Lat_abs + plant_height
    q1_median_mode ~ variance_q3_mean + skew_kurt
  
'
sem_fit <- sem(sem_mod, data=herbvar_scaled)
summary(sem_fit)

lavaanPlot(model = sem_fit,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "darkgreen"), coefs = TRUE, 
           covs = FALSE)
summary(sem_fit, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

### SEM Model 2: using factors made with FA, without interactions bw #########

sem_mod2 =   '
  # observed outcomes to observed predictors
    q1_median_mode + variance_q3_mean + skew_kurt ~ Lat_abs + plant_height
  
'
sem_fit2 <- sem(sem_mod2, data=herbvar_scaled)
summary(sem_fit2)

lavaanPlot(model = sem_fit2,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "darkgreen"), coefs = TRUE, 
           covs = FALSE)
summary(sem_fit2, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

### SEM Model 3: using quantiles instead of factors #########

sem_mod3 =   '
  # observed outcomes to observed predictors
    vari + skew + median ~ Lat_abs + plant_height
'
sem_fit3 <- sem(sem_mod3, data=herbvar_scaled)
summary(sem_fit3)

lavaanPlot(model = sem_fit3,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "darkgreen"), coefs = TRUE, 
           covs = FALSE)
summary(sem_fit3, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

### SEM Model 4: using CFA within lavaan to make factors similar to FA #########

sem_mod_4 =   '
  # observed outcomes to observed predictors
    center =~ first_q + median + mode 
    spread=~ vari + third_q + mean
    shape =~ skew + kurt
    
    distribution =~ center + spread + shape
    distribution ~ plant_height + Lat_abs
'
cfa_fit_4 <- cfa(sem_mod_4, data=herbvar_scaled)
summary(cfa_fit_4)

lavaanPlot(model = cfa_fit_4,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "darkgreen"), coefs = TRUE, 
           covs = FALSE)
summary(cfa_fit_4, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

##### Bayesian SEM using blavaan #####
#bsem_fit_4 = bcfa(sem_mod_4, data = herbvar_scaled, mcmcfile = T)
summary(bsem_fit_4)

##### - View and edit priors #####
dpriors()

##### - Fit measures #####
blavInspect(bsem_fit_4, 'rhat')
fitMeasures(bsem_fit_4)

##### - Trace plots ####
plot(bsem_fit_4, pars = 1:9, plot.type = "trace")

##### comparing ML + BAYES ########

bind_cols(parameterEstimates(cfa_fit_4)[, 1:4], 
          parameterEstimates(bsem_fit_4)[, 4]) %>% 
  rename(ML = est, Bayes = ...5) %>% knitr::kable()


### SEM Model 5: making factors that based on FA with one less var ####

sem_mod_5 =   '
    center =~ median + mode
    spread =~ vari + mean
    shape =~ skew + kurt 
    center + shape + spread ~ Lat_abs + plant_height
'
cfa_fit_5 <- cfa(sem_mod_5, data=herbvar_scaled)
summary(cfa_fit_5)

lavaanPlot(model = cfa_fit_5,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "darkgreen"), coefs = TRUE, 
           covs = FALSE)
summary(cfa_fit_5, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

##### Bayesian model #####

bsem_fit_5 = bcfa(sem_mod_5, data = herbvar_scaled, mcmcfile = T)
summary(bsem_fit_5)

posterior_samples <- parameterEstimates(bsem_fit_5, standardized = TRUE, 
                                        level = 0.95, ci = TRUE, se = TRUE)
posterior_samples = posterior_samples %>%
  mutate(parameter = paste(lhs,op,rhs)) %>%
  arrange(est)

ggplot(data = posterior_samples,aes(x = reorder(parameter, est), y = est)) +
  geom_point(size = 3, color = "blue") +  # Plot posterior means
  geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper), width = 0.5) +  # Add credible intervals
  labs(title = "Posterior Estimates with 95% Credible Intervals", 
       x = "Parameter", 
       y = "Posterior Estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

###### View and edit priors ##
dpriors (nu = "gamma(1,0.5)", alpha = "gamma(1,0.5)", beta = "gamma(1,0.5)")

##### Fit measures ##
blavInspect(bsem_fit_5, 'rhat')
fitMeasures(bsem_fit_5, fit.measures = c("aic","bic","rmsea","chisq","logl"))

# Trace plots #
plot(bsem_fit_5, pars = 4:9, plot.type = "trace")


##### comparing ML + BAYES #

bind_cols(parameterEstimates(cfa_fit_5)[, 1:4], 
          parameterEstimates(bsem_fit_5)[, 4]) %>% rename(ML = est, Bayes = ...5) %>% knitr::kable()

### SEM Model 6: making factors that are more intuitive ##### 

sem_mod_6 =   '
    center =~ median + mode + mean
    spread =~ vari + IQR
    shape =~ skew + kurt 
    center + shape + spread ~ Lat_abs + plant_height
'
cfa_fit_6 <- cfa(sem_mod_6, data=herbvar_scaled)
summary(cfa_fit_6)

lavaanPlot(model = cfa_fit_6,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "darkgreen"), coefs = TRUE, 
           covs = FALSE)
summary(cfa_fit_6, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

##### Bayesian model #####
options(mc.cores = parallel::detectCores())

bsem_fit_6 = bcfa(sem_mod_6, data = herbvar_scaled, mcmcfile = T)
summary(bsem_fit_6)

######## VIEW & EDIT PRIORS ##
dpriors()

##### FIT MEASURES #
blavInspect(bsem_fit_6, what = "list")
fitMeasures(bsem_fit_6, fit.measures = c("aic","bic","rmsea","chisq","logl"))

# trace plots #
plot(bsem_fit_6, pars = 1:4, plot.type = "trace")

# comparing ML + BAYES #

est_Bayes = parameterEstimates(bsem_fit_6)[, 4]
se_Bayes = as.numeric(bsem_fit_6@ParTable[["se"]])
est_Bayes_df <- data.frame(est_Bayes = est_Bayes, se_Bayes = se_Bayes)
se_ML = as.numeric(cfa_fit_6@ParTable[["se"]])

est_ML = parameterestimates(cfa_fit_6)[,1:4] %>%
  mutate(se_ML = se_ML) %>%
  mutate(est_Bayes = est_Bayes_df$est_Bayes) %>%
  mutate(se_Bayes = est_Bayes_df$se_Bayes)

results_table = as.data.frame(est_ML)
tt(results_table, theme = "striped", digits = 5) |> style_tt(j = 1, align = "c")

########## REFERENCES ############################


############## ZERO INFLATED JAGS CODE #######################

model{
  
  # For the ones trick
  C <- 10000
  
  # for every observation
  for(i in 1:N){
    
    # define the logistic regression model, where w is the probability of occurance.
    # use the logistic transformation exp(z)/(1 + exp(z)), where z is a linear function
    logit(w[i]) <- zeta[i]
    zeta[i] <- gamma0 + gamma1*MPD[i] + gamma2*MTD[i] + gamma3*int[i] + gamma4*MPD[i]*int[i] + gamma5*MTD[i]*int[i]
    
    # define the gamma regression model for the mean. use the log link the ensure positive, non-zero mu
    mu[i] <- pow(eta[i], -1)
    eta[i] <- beta0 + beta1*MPD[i] + beta2*MTD[i] + beta3*int[i] + beta4*MPD[i]*int[i] + beta5*MTD[i]*int[i]
    
    # redefine the mu and sd of the continuous part into the shape and scale parameters
    shape[i] <- pow(mu[i], 2) / pow(sd, 2)
    rate[i] <- mu[i] / pow(sd, 2)
    
    # for readability, define the log-likelihood of the gamma here
    logGamma[i] <- log(dgamma(y[i], shape[i], rate[i]))
    
    # define the total likelihood, where the likelihood is (1 - w) if y < 0.0001 (z = 0) or
    # the likelihood is w * gammalik if y >= 0.0001 (z = 1). So if z = 1, then the first bit must be
    # 0 and the second bit 1. Use 1 - z, which is 0 if y > 0.0001 and 1 if y < 0.0001
    logLik[i] <- (1 - z[i]) * log(1 - w[i]) + z[i] * ( log(w[i]) + logGamma[i] )
    
    Lik[i] <- exp(logLik[i])
    
    # Use the ones trick
    p[i] <- Lik[i] / C
    ones[i] ~ dbern(p[i])
  }
  
  # PRIORS
  beta0 ~ dnorm(0, 0.0001)
  beta1 ~ dnorm(0, 0.0001)
  beta2 ~ dnorm(0, 0.0001)
  beta3 ~ dnorm(0, 0.0001)
  beta4 ~ dnorm(0, 0.0001)
  beta5 ~ dnorm(0, 0.0001)
  
  gamma0 ~ dnorm(0, 0.0001)
  gamma1 ~ dnorm(0, 0.0001)
  gamma2 ~ dnorm(0, 0.0001)
  gamma3 ~ dnorm(0, 0.0001)
  gamma4 ~ dnorm(0, 0.0001)
  gamma5 ~ dnorm(0, 0.0001)
  
  sd ~ dgamma(2, 2)
  
}

################## BLAVAAN EXAMPLE #######

### SIMULATION #

# setup
J <- 1000
I <- 6
K <- 2
psi <- matrix(c(1, 0.5,
                0.5, 0.8), nrow = K)  
beta <- seq(1, 2, by = .2)

# loading matrix
Lambda <- cbind(c(1, 1.5, 2, 0, 0, 0), c(0, 0, 0, 1, 1.5, 2))

# error covariance
Theta <- diag(0.3, nrow = I)

# factor scores
eta <- mvrnorm(J, mu = c(0, 0), Sigma = psi)

# error term
epsilon <- mvrnorm(J, mu = rep(0, ncol(Theta)),Sigma = Theta)

dat <- tcrossprod(eta, Lambda) + epsilon
dat_cfa  <-  dat %>% as.data.frame() %>% setNames(c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6"))

lavaan_cfa <- 'eta1 =~ Y1 + Y2 + Y3
               eta2 =~ Y4 + Y5 + Y6'

FIT <-
  semPlotModel_lavaanModel(
    lavaan_cfa,
    auto.var = TRUE,
    auto.fix.first = TRUE,
    auto.cov.lv.x = TRUE
  )
semPaths(
  FIT,
  what = "paths",
  whatLabels = "par",
  ,
  nodeLabels = c(
    expression(paste(Y[1])),
    expression(paste(Y[2])),
    expression(paste(Y[3])),
    expression(paste(Y[4])),
    expression(paste(Y[5])),
    expression(paste(Y[6])),
    expression(paste(eta[1])),
    expression(paste(eta[2]))
  ),
  edge.label.cex = 0.8,
  edgeLabels = c(
    expression(paste(lambda[1])),
    expression(paste(lambda[2])),
    expression(paste(lambda[3])),
    expression(paste(lambda[4])),
    expression(paste(lambda[5])),
    expression(paste(lambda[6])),
    expression(paste("Covariance")),
    expression(paste(epsilon[1])),
    expression(paste(epsilon[2])),
    expression(paste(epsilon[3])),
    expression(paste(epsilon[4])),
    expression(paste(epsilon[5])),
    expression(paste(epsilon[6])),
    expression(paste(psi[1])),
    expression(paste(psi[2]))
  )
)

# blavaan
blav_cfa_fit <- bcfa(lavaan_cfa, data=dat_cfa, mcmcfile = T)

summary(blav_cfa_fit)

fitmeasures(blav_cfa_fit)

blavInspect(blav_cfa_fit, "mcobj")

(default_prior <- dpriors())

(new_prior <- dpriors(beta = "normal(0, 1)"))

#comparison

bind_cols(parameterEstimates(blav_cfa_fit)[, 1:4], parameterEstimates(blav_cfa_fit)[, 4]) %>% rename(ML = est, Bayes = ...5) %>% knitr::kable()
