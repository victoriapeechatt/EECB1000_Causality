###########################################################

pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
pkgTest("asbio")
pkgTest("moments")
pkgTest("lavaan")
pkgTest("lavaanPlot")
pkgTest("mice")
pkgTest("tidyverse")
pkgTest("psych")
pkgTest("blavaan")
pkgTest("bayesplot")
pkgTest("semPlot")
pkgTest("semptools")


options(mc.cores = parallel::detectCores())
future::plan("multisession")

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

herbvar = read.csv("pivoted.csv")
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
            skew = skewness(leaf_percent), 
            kurt = kurtosis(leaf_percent),
            cv = std/mean,
            MAD_median = mad(leaf_percent, center = median(leaf_percent)),
            MAD_mean = mad(leaf_percent, center = mean(leaf_percent), na.rm = TRUE),
            sarle_bimo = (skewness(leaf_percent)^2 + 1)/
                          (kurtosis(leaf_percent)+
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

herbvar_f = herbvar_f %>% 
  filter(!is.na(skew)) %>% 
  filter(!is.infinite(skew))
######### FACTOR ANALYSIS ########

vars = as.matrix(herbvar_f[,c(6,7,8,10,12,13,14,15)])
DH.test(herbvar_f[,c(5:15)], Y.names = NULL)

vars = apply(vars[,], MARGIN = 2, scale)


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

##########SEM MODELS ###############


herbvar_scaled = apply(herbvar_f[,c(3:26)], MARGIN = 2, scale)

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
coef(bsem_fit_5)

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

######## DAG 
semPaths(bsem_fit_5, what = "est", layout = "spring", edge.label.cex = 0.8,
         nCharNodes = 0, nCharEdges = 0, residuals = TRUE)

#final_semplot = mark_se(semplot,object = bsem_fit_5, sep = "\n", digits = 3)
#plot(final_semplot)

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

######## DAG 
semPaths(bsem_fit_6, what = "est", layout = "spring", edge.label.cex = 0.8,
         nCharNodes = 0, nCharEdges = 0, residuals = TRUE)

