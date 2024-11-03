### Necessary packages ####

library(tidyverse)
library(ggiraphExtra)
library(dagitty)
library(ggdag)
library(lavaan)
library(lavaanPlot)
library(gridExtra)


### Simulating data - hardcoding the negative effect of discrimination on females ####

tb <- tibble(
  female = ifelse(runif(10000)>=0.5,1,0),
  ability = rnorm(10000),
  discrimination = female,
  occupation = 1 + 2*ability + 0*female - 2*discrimination + rnorm(10000),
  wage = 1 - 1*discrimination + 1*occupation + 2*ability + rnorm(10000) 
)
tb

### Different linear regressions ###

fit1 <- lm(wage ~ female, tb)
fit2 <- lm(wage ~ female + occupation, tb)

equation1=function(x){coef(fit1)[2]+coef(fit1)[1]}
equation2=function(x){coef(fit2)[3]*x+coef(fit2)[2]+coef(fit2)[1]}
equation3=function(x){coef(fit1)[1]}
equation4=function(x){coef(fit2)[1]+coef(fit2)[3]*x}

### Plotting results ###

biased = 
  ggplot(data = tb, aes(x=occupation, y=wage, colour = female)) +
  geom_point(aes(x = occupation, y = wage, colour = female)) +
  stat_function(fun=equation2,geom="line",color="turquoise")+
  stat_function(fun=equation4,geom="line",color="darkblue")+
  guides(color=guide_legend(title="Male/Female"))+
  theme_grey(base_size = 28)
biased

### Adding in lines from regression without occupation as a variable ### 

biased + stat_function(fun=equation1,geom="line",color="turquoise")+
  stat_function(fun=equation3,geom="line",color="darkblue")

### Better plot to visualize the regression of females onto wages ###

ggplot(data = tb, aes(x=female, y=wage)) + geom_boxplot(aes(fill=female, group = female))

### Fit summaries with coefficients flipping in sign ###

summary(fit2) 
summary(fit1)

### Drawing out DAGs ### 
theme_set(theme_dag())
dag_inthe_bag <- dagify(wage ~ discrimination + occupation + ability,
                     occupation ~ discrimination + ability,
                     labels = c(
                       "wage" = "Wage",
                       "occupation" = "Occupation",
                       "discrimination" = "Discrimination",
                       "ability" = "Ability"
                     ),
                     exposure = "discrimination",
                     latent = "ability",
                     outcome = "wage")

#  automatically searches the paths between the variables labelled exposure and
#  outcome
dag_paths(dag_inthe_bag)
ggdag(dag_inthe_bag, text = FALSE, use_labels = "label")
ggdag_paths(dag_inthe_bag, text = FALSE, use_labels = "label", shadow = TRUE, node = FALSE)
ggdag_dconnected(dag_inthe_bag, from = "discrimination", to = "wage", controlling_for = "occupation")
### A better way to visualize causal relationships - The SEM ###

sem_mod =   '
  # observed outcomes to observed predictors
    wage ~ discrimination + occupation
    occupation ~ discrimination 
  
'
sem_fit <- sem(sem_mod, data=tb)
summary(sem_fit)

lavaanPlot(model = sem_fit,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = TRUE)
summary(sem_fit, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

sem_mod2 =   '
  # observed outcomes to observed predictors
    wage ~ discrimination 
  
'
sem_fit2 <- sem(sem_mod2, data=tb)
summary(sem_fit2)

lavaanPlot(model = sem_fit2,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = TRUE)
summary(sem_fit2, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

### Conditional independence with code ##### 


dag <- dagify(
  x ~ e1 + z,
  y ~ z + e2, 
  exposure = "x",
  outcome = "y",
  coords = list(x = c(e1 = 1, x = 2, z = 3, y = 4, e2 = 5),
                y = c(e1 = 1.5, x = 1, z = 1, y = 1, e2 = 1.5))
)
ggdag_status(dag)
ggdag(dag, layout = "nicely")

set.seed(314)
data = data.frame(
  e1 = rnorm(10000, 0, .19),
  e2 = rnorm(10000, 0, .19),
  z = rnorm(10000, 0, 1),
  y = 0.9*z + e1,
  x = 0.9*z + e2
)

plot(x,y)
cov(x,y)
cov(x,z)
cov(y,z)

filt0 = ggplot(data = data) +
  geom_point(mapping = aes(x,y, fill = "black"))
cov(data$x, data$y)

data.filtered1 = data %>%
  filter(z < 2 & z > (-2))
filt1 = ggplot(data = data.filtered1) +
  geom_point(mapping = aes(x,y, fill = "black"))

data.filtered2 = data %>%
  filter(z< (0.5) & z > (-.5))
filt2 = ggplot(data = data.filtered2) +
  geom_point(mapping = aes(x,y, fill = "black"))
cov(data.filtered2$x, data.filtered2$y)

grid.arrange(filt0, filt1, filt2, ncol = 3)

