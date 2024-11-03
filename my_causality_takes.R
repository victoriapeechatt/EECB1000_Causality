library(dagitty)
library(ggdag)
library(ggpubr)
library(gridExtra)

dag_inthe_bag <- dagify(
  plant_size ~ fertilizer + light,
  herbivory ~ plant_size + foraging + light,
  foraging ~ light,
  exposure = "fertilizer",
  outcome = "herbivory",
  labels = c(
    "fertilizer" = "fertilizer",
    "herbivory" = "herbivory",
    "plant_size" = "plant size",
    "light" = "light",
    "foraging" = "foraging"
  )
)%>% tidy_dagitty()

theme_set(theme_dag())
ggdag(dag_inthe_bag, layout = "nicely", 
      node = FALSE, 
      label_col = "black", 
      text_col = "black")


ggdag_adjustment_set(dag_inthe_bag, 
                     exposure = "fertilizer",
                     outcome = "herbivory",
                     node_size = 25,
                     shadow = TRUE)

ggdag_dconnected(dag_inthe_bag, 
                 from = "fertilizer",
                 to = "herbivory", 
                 controlling_for = "plant_size",
                 node_size = 25,
                 edge_type = "link")

ggdag_dconnected(dag_inthe_bag, 
                 from = "fertilizer",
                 to = "herbivory", 
                 controlling_for = "light",
                 node_size = 25,
                 edge_type = "link")

ggdag_dconnected(dag_inthe_bag, 
                 from = "fertilizer",
                 to = "herbivory", 
                 controlling_for = "foraging",
                 node_size = 25,edge_type = "link")

library(lavaan)
library(lavaanPlot)
library(lme4)
library(piecewiseSEM)

data = read.csv("Documents/GitHub/EECB1000_Causality/data.csv")
CC_dat <- data %>%
  mutate(lag_plant_size = lag(plant_size, default = NA))

fertilizer = data$fertilizer
light = data$light
plant_size = data$plant_size
foraging = data$foraging
herbivory = data$herbivory

pairs(data)

glm1 = glm(herbivory ~ foraging+light+plant_size+fertilizer, data = data, family = "gaussian")

summary(glm1)

CC_psem_model <- psem(glm1, glm2)
CC_psem_model
summary(CC_psem_model)
plot(CC_psem_model)

sem_mod1 =   '
  # observed outcomes to observed predictors
    plant_size ~ fertilizer + light
    herbivory ~ plant_size + foraging + light
    foraging ~ light
'
sem_fit1 <- sem(sem_mod1, data=data)
summary(sem_fit1)
standardizedsolution(sem_fit1)

lavaanPlot(model = sem_fit1,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = TRUE)
summary(sem_fit1, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

sem_mod1.5 =   '
  # observed outcomes to observed predictors
    plant_size ~ fertilizer +  light
    herbivory ~ plant_size + foraging + light
    foraging ~ light
  '
sem_fit1.5 <- sem(sem_mod1.5, data=data)
summary(sem_fit1.5)

lavaanPlot(model = sem_fit1.5,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = TRUE)
summary(sem_fit1.5, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

sem_mod1.6 =   '
  # Mediaton / Moderation effect 
    plant_size ~ a*fertilizer + light
    herbivory ~ foraging + light  + c*fertilizer
    foraging ~ light + b*plant_size
    Med :=  a*b
    Total_mod :=  Med + c
  
'
sem_fit1.6 <- sem(sem_mod1.6, data=data)
summary(sem_fit1.6)

lavaanPlot(model = sem_fit1.6,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = TRUE)
summary(sem_fit1.6, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

sem_mod1.9 =   '
  # observed outcomes to observed predictors
    plant_size ~ a*fertilizer + light
    herbivory ~ b*plant_size + foraging + light + c*fertilizer
    foraging ~ light
  
'
sem_fit1.9 <- sem(sem_mod1.9, data=data)
summary(sem_fit1.9)

lavaanPlot(model = sem_fit1.9,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = TRUE)
summary(sem_fit1.9, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)



sem_mod2 =   '
  # observed outcomes to observed predictors
    plant_size ~ fertilizer + light 
    herbivory ~ plant_size + foraging + light
    foraging ~ light + plant_size
  
'
sem_fit2 <- sem(sem_mod2, data=data)
summary(sem_fit2)

lavaanPlot(model = sem_fit2,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = TRUE)
summary(sem_fit2, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

rmarkdown::render(input = "my_causality_takes.R")
