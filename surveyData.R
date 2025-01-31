pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

pkgTest("data.table")
pkgTest("inspectdf")
pkgTest("tidyverse")
pkgTest("lubridate")
pkgTest("gridExtra")
pkgTest("grid")
pkgTest("ggpubr")
pkgTest("lme4")
pkgTest("nlme")
pkgTest("sjPlot")
pkgTest("forcats")
pkgTest("vegan")
pkgTest("lmtest")
pkgTest("emmeans")
pkgTest("asbio")
pkgTest("chisq.posthoc.test")
pkgTest("lavaan")
pkgTest("lavaanPlot")
pkgTest("rstan")
pkgTest("shinystan")
pkgTest("ggpmisc")

data = read.csv("Documents/GitHub/EECB1000_Causality/surveyData_2024-11-15.csv")
str(data)

cleandata <- data %>% 
  select(phase,date, surveyIteration, 
         site, country, Lat, Lat_abs, Long, Lat_abs_scale, hemi, elevation, 
         propLeavesHerbMean, propPlantsHerb50, propPlantsHerb80, varAmtEaten, 
         GiniAmtEaten, meanAmtEaten, 
         plantHeight_cmMean, sizeRadiusMean, sizeDiameterMean,
         plantMean, plantMedian, plantVar, plantSD, plantSkew, plantGini, 
         leafMean, leafVar, leafSD, leafSkew,leafGini
         ) %>%
  mutate(date = ymd(date)
  )

cleandata$jdate = format(cleandata$date, "%j")

inspectdf::inspect_types(cleandata)
inspectdf::inspect_cat(cleandata)
inspectdf::show_plot(inspectdf::inspect_cat(cleandata))

str(cleandata)

causality_Data = cleandata %>% 
  mutate(date = as.numeric(jdate)) %>%
  select(date,
         Lat_abs_scale, elevation, 
         propLeavesHerbMean, propPlantsHerb50, propPlantsHerb80, varAmtEaten, 
         GiniAmtEaten, meanAmtEaten, 
         plantHeight_cmMean, sizeRadiusMean, sizeDiameterMean,
         plantMean, plantMedian, plantVar, plantSD, plantSkew, plantGini, 
         leafMean, leafVar, leafSD, leafSkew,leafGini
  )

str(causality_Data)

#######################
## Questions: 
## - What causes changes in herbivory? 
## Hypothesis: 
## 1 - Latitude results in higher variance of herbivory in upper lat
## 2 - plant size results in higher mean herbivory of larger plants 
## 
## Causal structure: 
## Variables pointing to herbivory 
## herbivory as a distribution pointint to 
## mean
## standard deviation 
## skew 

write_csv(causality_Data, file = "causality_data.csv")

############################################3
## AFTER SURD 

# Step 1: Read the data from a CSV file
# Replace 'path_to_your_file.csv' with the actual path to your CSV file
df <- read.csv("SURD/results/plantgini1.csv")

# Check the structure of the data to make sure it loaded correctly
head(df)
str(df)
summary(df)
# Signal 1 

df_filtered <- df %>%
  filter(Signal == 1,Value > 0)

df_top3 <- df_filtered %>%
  top_n(5, wt = Value) %>%  # Select the top 3 values across the entire dataset
  arrange(desc(Value))

# Create a text
grob <- grobTree(textGrob("
                    1 - meanAmtEaten,
                    2 - propLeavesHerbMean,
                    3- Lat_abs_scale,
                    4 - plantHeight_cmMean,
                    5 - varAmtEaten,
                    6 - GiniAmtEaten,
                    7 - plantMean,
                    8 - plantMedian,
                    9 - plantVar, 
                    10 - plantSD, 
                    11 - plantSkew", x=0.95,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=10, fontface="italic")))

# Step 3: Create the plot
ggplot(df_filtered, aes(x = Key, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set2") +  # Use a color palette for Combo
  theme_minimal() +
  facet_wrap(~Type, scales = "free")+
  labs(x = "Combo", y = "Value", title = "Information to predict plantgini - 1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  annotation_custom(grob)

##################################################################

############################################3
## AFTER SURD 

# Step 1: Read the data from a CSV file
# Replace 'path_to_your_file.csv' with the actual path to your CSV file
df <- read.csv("SURD/results/varAmtEaten1.csv")

# Check the structure of the data to make sure it loaded correctly
head(df)
str(df)
summary(df)
# Signal 1 

df_filtered <- df %>%
  filter(Signal == 1,Value > 0.001)

df_top3 <- df_filtered %>%
  top_n(5, wt = Value) %>%  # Select the top 3 values across the entire dataset
  arrange(desc(Value))

# Create a text
grob <- grobTree(textGrob("
                    1 - meanAmtEaten,
                    2 - propLeavesHerbMean,
                    3 - Lat_abs_scale,
                    4 - plantHeight_cmMean,
                    5 - GiniAmtEaten,
                    6 - plantGini,
                    7 - plantMean,
                    8 - plantMedian,
                    9 - plantVar, 
                    10 - plantSD, 
                    11 - plantSkew", x=0.95,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=10, fontface="italic")))

# Step 3: Create the plot
ggplot(df_filtered, aes(x = Key, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set2") +  # Use a color palette for Combo
  theme_minimal() +
  facet_wrap(~Type, scales = "free")+
  labs(x = "Combo", y = "Value", title = "Information to predict varAmtEaten - 1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##################################################################

############################################3
## AFTER SURD 

# Step 1: Read the data from a CSV file
# Replace 'path_to_your_file.csv' with the actual path to your CSV file
df <- read.csv("SURD/results/varAmtEaten2.csv")

# Check the structure of the data to make sure it loaded correctly
head(df)
str(df)
summary(df)
# Signal 1 

df_filtered <- df %>%
  filter(Signal == 1,Value > 0.001)

df_top3 <- df_filtered %>%
  top_n(5, wt = Value) %>%  # Select the top 3 values across the entire dataset
  arrange(desc(Value))

# Create a text
grob <- grobTree(textGrob("
                    1 - meanAmtEaten,
                    2 - Lat_abs_scale,
                    3 - plantHeight_cmMean,
                    4 - GiniAmtEaten", x=0.2,  y=0.95, hjust=0,
                          gp=gpar(col="black", fontsize=10, fontface="italic")))

# Step 3: Create the plot
ggplot(df_filtered, aes(x = Key, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set2") +  # Use a color palette for Combo
  theme_minimal() +
  facet_wrap(~Type, scales = "free")+
  labs(x = "Combo", y = "Value", title = "Information to predict varAmtEaten - 1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  annotation_custom(grob)
##################################################################

############################################3
## AFTER SURD 

# Step 1: Read the data from a CSV file
# Replace 'path_to_your_file.csv' with the actual path to your CSV file
df <- read.csv("SURD/results/meanAmtEaten2.csv")

# Check the structure of the data to make sure it loaded correctly
head(df)
str(df)
summary(df)
# Signal 1 

df_filtered <- df %>%
  filter(Signal == 1,Value > 0.001)

df_top3 <- df_filtered %>%
  top_n(5, wt = Value) %>%  # Select the top 3 values across the entire dataset
  arrange(desc(Value))

# Create a text
grob <- grobTree(textGrob("
                    1 - varAmtEaten,
                    2 - Lat_abs_scale,
                    3 - plantHeight_cmMean,
                    4 - GiniAmtEaten", x=0.2,  y=0.8, hjust=0,
                          gp=gpar(col="black", fontsize=10, fontface="italic")))

# Step 3: Create the plot
ggplot(df_filtered, aes(x = Key, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set2") +  # Use a color palette for Combo
  theme_minimal() +
  facet_wrap(~Type, scales = "free")+
  labs(x = "Combo", y = "Value", title = "Information to predict meanAmtEaten - 1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  annotation_custom(grob)
##################################################################

############################################3
## AFTER SURD 

# Step 1: Read the data from a CSV file
# Replace 'path_to_your_file.csv' with the actual path to your CSV file
df <- read.csv("SURD/results/giniAmtEaten2.csv")

# Check the structure of the data to make sure it loaded correctly
head(df)
str(df)
summary(df)
# Signal 1 

df_filtered <- df %>%
  filter(Signal == 2,Value > 0.001)

df_top3 <- df_filtered %>%
  top_n(5, wt = Value) %>%  # Select the top 3 values across the entire dataset
  arrange(desc(Value))

# Create a text
grob <- grobTree(textGrob("
                    1 - meanAmtEaten,
                    2 - varAmtEaten,
                    3 - Lat_abs_scale,
                    4 - plantHeight_cmMean", x=0.2,  y=0.8, hjust=0,
                          gp=gpar(col="black", fontsize=10, fontface="italic")))

# Step 3: Create the plot
ggplot(df_filtered, aes(x = Key, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set2") +  # Use a color palette for Combo
  theme_minimal() +
  facet_wrap(~Type, scales = "free")+
  labs(x = "Combo", y = "Value", title = "Information to predict giniAmtEaten - 1",
       subtitle = "1 - meanAmtEaten\n2 - varAmtEaten\n3 - Lat_abs_scale\n4 - plantHeight_cmMean") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##################################################################
##        FULLL LEAF DATA @ PLNT LEVEL 
############################################3
## AFTER SURD 

# Step 1: Read the data from a CSV file
# Replace 'path_to_your_file.csv' with the actual path to your CSV file
df <- read.csv("SURD/results/CLEANER_leafmean.csv")

# Check the structure of the data to make sure it loaded correctly
head(df)
str(df)
summary(df)
# Signal 1 

df_filtered <- df %>%
  filter(Signal == 2,Value > 0.01)

df_top3 <- df_filtered %>%
  top_n(5, wt = Value) %>%  # Select the top 3 values across the entire dataset
  arrange(desc(Value))

# Step 3: Create the plot
ggplot(df_filtered, aes(x = Key, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set2") +  # Use a color palette for Combo
  theme_minimal() +
  facet_wrap(~Type, scales = "free")+
  labs(x = "Combo", y = "Value", title = "Information to predict leafmean - 1",
       subtitle = "0 - leafMean,
                    1 - leafSD,
                    2 - leafGini,
                    3 - leafSkew,
                    4 - leafKurt,
                    5 - leafCV,
                    6 - propHerb,
                    7 - Lat,
                    8 - Long") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############################################3
## AFTER SURD 

# Step 1: Read the data from a CSV file
# Replace 'path_to_your_file.csv' with the actual path to your CSV file
df <- read.csv("SURD/results/CLEANER_propherb.csv")

# Check the structure of the data to make sure it loaded correctly
head(df)
str(df)
summary(df)
# Signal 1 

df_filtered <- df %>%
  filter(Signal == 2,Value > 0.001)

df_top3 <- df_filtered %>%
  top_n(5, wt = Value) %>%  # Select the top 3 values across the entire dataset
  arrange(desc(Value))

# Step 3: Create the plot
ggplot(df_filtered, aes(x = Key, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set2") +  # Use a color palette for Combo
  theme_minimal() +
  facet_wrap(~Type, scales = "free")+
  labs(x = "Combo", y = "Value", title = "Information to predict propherb - 1",
       subtitle = "0 - propHerb, 
                    1 - leafMean, 
                    2 - leafSD,
                    3 - leafGini,
                    4 - leafSkew,
                    5 - leafKurt,
                    6 - leafCV,
                    7 - Lat,
                    8 - Long") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############################################3
## AFTER SURD 

# Step 1: Read the data from a CSV file
# Replace 'path_to_your_file.csv' with the actual path to your CSV file
df <- read.csv("SURD/results/CLEANER_amteaten.csv")

# Check the structure of the data to make sure it loaded correctly
head(df)
str(df)
summary(df)
# Signal 1 

df_filtered <- df %>%
  filter(Signal == 2,Value > 0.0001)

df_top3 <- df_filtered %>%
  top_n(5, wt = Value) %>%  # Select the top 3 values across the entire dataset
  arrange(desc(Value))

# Step 3: Create the plot
ggplot(df_filtered, aes(x = Key, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set2") +  # Use a color palette for Combo
  theme_minimal() +
  facet_wrap(~Type, scales = "free")+
  labs(x = "Combo", y = "Value", title = "Information to predict amteaten - 1",
       subtitle = "0 - amtEaten, 
                    1 - leafMean, 
                    2 - leafSD,
                    3 - leafGini,
                    4 - leafSkew,
                    5 - leafKurt,
                    6 - leafCV,
                    7 - Lat,
                    8 - Long") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

