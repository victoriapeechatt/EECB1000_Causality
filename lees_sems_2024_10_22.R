#################################
## Data Simulation with Correlated Light and Fertilizer
#################################
# Here I've simulated correlated light and fertilizer and generate data with other correlated variables
# Plant size (C) is influenced by both light and fertilizer (making it a collider)
# Herbivory is affected directly by light
set.seed(69)
n <- 1000
correlation <- 0.5  
cov_matrix <- matrix(c(1, correlation, correlation, 1), nrow = 2)
correlated_data <- MASS::mvrnorm(n, mu = c(0, 0), Sigma = cov_matrix)
light <- correlated_data[, 1]
fertilizer <- correlated_data[, 2]
plant_size <- 0.5 * light + 0.5 * fertilizer + rnorm(n)
foraging <- 0.7 * light + rnorm(n)
herbivory <- 0.5 * plant_size + 0.8 * foraging + 0.3 * light + rnorm(n)
data <- data.frame(fertilizer = fertilizer, light = light, plant_size = plant_size, foraging = foraging, herbivory = herbivory)
head(data)

###some 
library(corrgram)
library(lavaan)
library(lavaanPlot)
library(blavaan)
library(ggplot2)


#########################################################################################
## Correlation Matrix 
#######################################################
corrgram(data, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt,
         main = "Correlation Matrix of Variables")
cor_matrix <- cor(data)
print(cor_matrix)


#################################
## SEM Model 1: Plant Size as a Collider
##note, this is not really an issue as it is in the multiple regression below, it is just a different causal structure
#################################

model_condition_collider <- '
  plant_size ~ fertilizer + light     #  (collider)
  foraging ~ light                    
  herbivory ~ plant_size + foraging + light  
  fertilizer ~ light                   
'
fit_condition_collider <- sem(model_condition_collider, data = data)
summary(fit_condition_collider, standardized = TRUE)
coef_values1 <- coef(fit_condition_collider)
##this is "do-calculus" for all values - put whatever value you'd like here
predicted_herbivory_collider <- coef_values1["herbivory~light"] * data$light +
  coef_values1["herbivory~plant_size"] * (coef_values1["plant_size~light"] * data$light) +
  coef_values1["herbivory~foraging"] * (coef_values1["foraging~light"] * data$light)
head(predicted_herbivory_collider)

lavaanPlot(model = fit_condition_collider,  
           node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "darkgreen"), coefs = TRUE, covs = FALSE, stand=TRUE)


##HERE IS THE FULL EFFECT OF LIGHT ON HERBIVORY (standardized coefficients)
standardized_coef <- standardizedSolution(fit_condition_collider)
direct_effect_light_herbivory_std <- standardized_coef[standardized_coef$lhs == "herbivory" & 
                                                         standardized_coef$rhs == "light", "est.std"]
indirect_effect_light_plant_size_herbivory_std <- standardized_coef[standardized_coef$lhs == "plant_size" & 
                                                                      standardized_coef$rhs == "light", "est.std"] *
  standardized_coef[standardized_coef$lhs == "herbivory" & 
                      standardized_coef$rhs == "plant_size", "est.std"]
indirect_effect_light_foraging_herbivory_std <- standardized_coef[standardized_coef$lhs == "foraging" & 
                                                                    standardized_coef$rhs == "light", "est.std"] *
  standardized_coef[standardized_coef$lhs == "herbivory" & 
                      standardized_coef$rhs == "foraging", "est.std"]
overall_effect_light_herbivory_std <- direct_effect_light_herbivory_std +
  indirect_effect_light_plant_size_herbivory_std +
  indirect_effect_light_foraging_herbivory_std
overall_effect_light_herbivory_std

#######Interactions calculated in class 14-october###########
data$FTinteract <- data$fertilizer*data$light
modelInteract1 <- '
  plant_size ~ FTinteract     
  foraging ~ FTinteract                    
  herbivory ~ plant_size + foraging + FTinteract  
                    
'
fit_interact1 <- sem(modelInteract1, data = data)
summary(fit_interact1, standardized = TRUE)
lavaanPlot(model = fit_interact1,  
           node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "darkgreen"), coefs = TRUE, covs = FALSE, stand=TRUE)
###############################################################


#################################
## SEM Model 2: Plant Size not a collider - no arrow from Fertilizer to Plant Size
#################################

model_no_collider <- '
  plant_size ~ light                   
  foraging ~ light                     
  herbivory ~ plant_size + foraging + light  
  fertilizer ~ light                   
'
fit_no_collider <- sem(model_no_collider, data = data)
summary(fit_no_collider, standardized = TRUE)
coef_values2 <- coef(fit_no_collider)
##this is "do-calculus" for all values - put whatever value you'd like here
predicted_herbivory_NOcollider <- coef_values2["herbivory~light"] * data$light +
  coef_values2["herbivory~plant_size"] * (coef_values2["plant_size~light"] * data$light) +
  coef_values2["herbivory~foraging"] * (coef_values2["foraging~light"] * data$light)
head(predicted_herbivory_NOcollider)

lavaanPlot(model = fit_no_collider,  
           node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "darkgreen"), coefs = TRUE, covs = FALSE, stand=TRUE)


#################################
## Multiple Regression: Estimating the Effect of Light on Herbivory, conditioning on all other variables (i.e. conditioning on collider)
#################################
multiple_regression_model1 <- lm(herbivory ~ light + plant_size + foraging + fertilizer, data = data)
summary(multiple_regression_model1)

##PARTIALS FROM THIS MODEL
# LIGHT (controlling for ALL others)
ggplot(data, aes(x = light, y = residuals(lm(herbivory ~ plant_size + foraging + fertilizer, data = data)))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Partial Regression: Light on Herbivory", 
       x = "Light", 
       y = "Residual Herbivory (controlling for Plant Size, Foraging, Fertilizer)") +
  theme_minimal()

# Plant size (controlling for ALL others)
ggplot(data, aes(x = plant_size, y = residuals(lm(herbivory ~ light + foraging + fertilizer, data = data)))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Partial Regression: Plant Size on Herbivory", 
       x = "Plant Size", 
       y = "Residual Herbivory (controlling for Light, Foraging, Fertilizer)") +
  theme_minimal()

# Foraging (controlling for ALL others)
ggplot(data, aes(x = foraging, y = residuals(lm(herbivory ~ light + plant_size + fertilizer, data = data)))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Partial Regression: Foraging on Herbivory", 
       x = "Foraging", 
       y = "Residual Herbivory (controlling for Light, Plant Size, Fertilizer)") +
  theme_minimal()

# Fertilizer (controlling for ALL others)
ggplot(data, aes(x = fertilizer, y = residuals(lm(herbivory ~ light + plant_size + foraging, data = data)))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Partial Regression: Fertilizer on Herbivory", 
       x = "Fertilizer", 
       y = "Residual Herbivory (controlling for Light, Plant Size, Foraging)") +
  theme_minimal()


#################################
## Multiple Regression: Estimating the Effect of Light on Fertilizer (showing the bias from conditioning on collider)
#################################
multiple_regression_model <- lm(fertilizer ~ light + plant_size + foraging + herbivory, data = data)
summary(multiple_regression_model)


####################MODEL VARIATIONS############################################
## INDIRECT EFFECT MODELED
#################################

library(mediation)
##MEDIATION APPROACH
mediator_model <- lm(plant_size ~ fertilizer + light, data = data)
foraging_model <- lm(foraging ~ light, data = data)
outcome_model <- lm(herbivory ~ plant_size * fertilizer + foraging + light, data = data)
med_fit <- mediate(mediator_model, outcome_model, 
                   treat = "fertilizer", mediator = "plant_size", 
                   boot = TRUE, sims = 1000)  
summary(med_fit)
plot(med_fit)



###############################
##Interaction modeled
#############################

interaction_term <- fertilizer * light
plant_size <- 0.5 * light + 0.5 * fertilizer + 0.3 * interaction_term + rnorm(n)
data <- data.frame(fertilizer = fertilizer, light = light, plant_size = plant_size, foraging = foraging, herbivory = herbivory)

data$FTinteract <- data$fertilizer*data$light
model_interaction <- '
  plant_size ~ FTinteract + fertilizer + light    
  foraging ~ light                    
  herbivory ~ plant_size + foraging + light  
  '
fit_interact <- sem(model_interaction, data = data)
summary(fit_interact, standardized = TRUE)

# Plot SEM model 
lavaanPlot(model = fit_interact,  
           node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "darkgreen"), coefs = TRUE, covs = FALSE, stand=TRUE)


##############Partial plots
# Fertilizer on plant size controlling for light
ggplot(data, aes(x = fertilizer, y = plant_size)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = FALSE, color = "blue") +
  labs(title = "Partial Correlation: Fertilizer on Plant Size (controlling for Light)", 
       x = "Fertilizer", 
       y = "Plant Size (Biomass)") +
  theme_minimal()

# Light on plant size controlling for fertilizer
ggplot(data, aes(x = light, y = plant_size)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = FALSE, color = "blue") +
  labs(title = "Partial Correlation: Light on Plant Size (controlling for Fertilizer)", 
       x = "Light", 
       y = "Plant Size (Biomass)") +
  theme_minimal()


#######NOW HERE IS A VERSION WHERE YOU CAN SEE THIS INTERACTION VIA CONDITIONING ON FERTILIZER:
# fertilizer strata are low, medium, high based on quantiles
data$fertilizer_group <- cut(data$fertilizer, 
                             breaks = quantile(data$fertilizer, probs = c(0, 0.33, 0.67, 1)), 
                             labels = c("low", "medium", "high"), 
                             include.lowest = TRUE)
model_interaction <- '
  plant_size ~ light    
  foraging ~ light                    
  herbivory ~ plant_size + foraging + light  
'
fit_low <- sem(model_interaction, data = subset(data, fertilizer_group == "low"))
summary(fit_low, standardized = TRUE)
fit_medium <- sem(model_interaction, data = subset(data, fertilizer_group == "medium"))
summary(fit_medium, standardized = TRUE)
fit_high <- sem(model_interaction, data = subset(data, fertilizer_group == "high"))
summary(fit_high, standardized = TRUE)

###some poor graphs showing the interaction
coef_low <- standardizedSolution(fit_low)
coef_medium <- standardizedSolution(fit_medium)
coef_high <- standardizedSolution(fit_high)
coef_comparison <- rbind(data.frame(coef_low, group = "low"),
                         data.frame(coef_medium, group = "medium"),
                         data.frame(coef_high, group = "high"))
ggplot(coef_comparison, aes(x = lhs, y = est.std, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Standardized Effects by Fertilizer Group", x = "Variable", y = "Standardized Coefficients") +
  theme_minimal()

##############Bayesian version - this takes a little longer 
fit_low_bayes <- bsem(model_interaction, data = subset(data, fertilizer_group == "low"), burnin = 2000, sample = 10000)
summary(fit_low_bayes)
fit_medium_bayes <- bsem(model_interaction, data = subset(data, fertilizer_group == "medium"), burnin = 2000, sample = 10000)
summary(fit_medium_bayes)
fit_high_bayes <- bsem(model_interaction, data = subset(data, fertilizer_group == "high"), burnin = 2000, sample = 10000)
summary(fit_high_bayes)

post_low <- blavInspect(fit_low_bayes, "samples")
post_medium <- blavInspect(fit_medium_bayes, "samples")
post_high <- blavInspect(fit_high_bayes, "samples")

# Extract posterior samples for 'plant_size ~ light' (or whatever you want here) and put into graphing dataframe and graph
post_low_plant_light <- as.vector(do.call(rbind, lapply(post_low, function(chain) chain[, "bet_sign[3]"])))
post_medium_plant_light <- as.vector(do.call(rbind, lapply(post_medium, function(chain) chain[, "bet_sign[3]"])))
post_high_plant_light <- as.vector(do.call(rbind, lapply(post_high, function(chain) chain[, "bet_sign[3]"])))
post_samples <- data.frame(
  value = c(post_low_plant_light, post_medium_plant_light, post_high_plant_light),
  group = factor(rep(c("low", "medium", "high"), each = length(post_low_plant_light)))
)
ggplot(post_samples, aes(x = group, y = value, fill = group)) +
  geom_boxplot() +
  labs(title = "Posterior Distribution of 'plant_size ~ light' Coefficients", 
       x = "Fertilizer Group", y = "Posterior Estimate") +
  theme_minimal()

