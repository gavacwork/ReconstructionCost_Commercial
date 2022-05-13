# Exploration reconstruction cost CGEN policies
# By Gabrielle Vachon
# March 2022

library(here)
library(dplyr)
library(qs)
library(ggplot2)
library(caret)
library(modEvA)
library(boot)
library(reshape2)

options(scipen = 100)


# Load data ---------------------------------------------------------------

cgen_pol <- qread(here("data/input/cgen_policies.qs"))
test <- extraw::get_atgl()


# Data Overview -----------------------------------------------------------

summary(cgen_pol$Building_limit)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 350000  1200000  1674000  2238227  2300000 40285000

table(cgen_pol$PRODUIT, useNA = "always")
# MCO  MPF <NA> 
#   1289   96    0    

table(cgen_pol$PROD_15183)  # professional eval condo
# O 
# 1291

summary(cgen_pol$PROD_SUPERREZ) # ground floor area
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#       1    1740    2501    3409    3750  321328      11 

table(cgen_pol$PROD_UMESSUPE, useNA = "always") # ground floor area unit
# ME   PI <NA> 
#   55 1319   11 

summary(cgen_pol$PROD_SUTOOCCO) # total occupied area
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#       0    5170    7671   10264   10802  130501     173 

table(cgen_pol$PROD_UMESSUP2, useNA = "always") # total occupied area unit
# ME   PI <NA> 
#   53 1191  141

table(cgen_pol$PROD_NBETEXSS, useNA = "always") # number of stories
# 1    2    3    4    5    6    8   10 <NA> 
#   48  644  617   58    3    7    3    2    3 

table(cgen_pol$PROD_SOUSOLON, useNA = "always") # basement indicator
# NO  YES <NA> 
#   344 1038    3 

summary(cgen_pol$PROD_ANNEE) # building year
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1800    1994    2006    2001    2012    2022 





# Univariate Analyses -----------------------------------------------------

# condos vs appartment
cgen_pol %>% 
  filter(Building_limit < 5000000) %>%  # remove bigger values for better visual
  ggplot(aes(x = PRODUIT, y = Building_limit)) +
  geom_boxplot() +
  theme_bw()

# ground floor area
# convet everything in PI
cgen_pol <- cgen_pol %>% 
  mutate(SUPERFICIE_UNIF = case_when(PROD_UMESSUPE == "ME" ~ PROD_SUPERREZ * 3.28084,
                                     TRUE ~ PROD_SUPERREZ))

cgen_pol %>% 
  filter(SUPERFICIE_UNIF < 25000) %>%  # remove outliers
  ggplot(aes(x = SUPERFICIE_UNIF, y = Building_limit)) +
  geom_point() +
  theme_bw()
           
# total occupied area 
# convet everything in PI
cgen_pol <- cgen_pol %>% 
  mutate(OCCUPPIED_UNIF = case_when(PROD_UMESSUP2 == "ME" ~ PROD_SUTOOCCO * 3.28084,
                                     TRUE ~ PROD_SUTOOCCO))

cgen_pol %>% 
  filter(OCCUPPIED_UNIF < 200000) %>%  # remove outliers
  ggplot(aes(x = OCCUPPIED_UNIF, y = Building_limit)) +
  geom_point() +
  theme_bw()

# number of stories
cgen_pol %>% 
  mutate(PROD_NBETEXSS = as.factor(PROD_NBETEXSS)) %>% 
  filter(Building_limit < 5000000) %>%  # remove bigger values for better visual
  ggplot(aes(x = PROD_NBETEXSS, y = Building_limit)) +
  geom_boxplot() +
  theme_bw()

# basement
cgen_pol %>% 
  filter(Building_limit < 5000000) %>%  # remove bigger values for better visual
  ggplot(aes(x = PROD_SOUSOLON, y = Building_limit)) +
  geom_boxplot() +
  theme_bw()

# building limit
cgen_pol %>% 
  ggplot(aes(x=Building_limit)) +
  geom_density()




# Data split ------------------------------------------------------------

# Define the partition (75% of the data for training)
set.seed(123456)
trainIndex <- createDataPartition(cgen_pol$Building_limit, p = .75, 
                                  list = FALSE, 
                                  times = 1)

# Split the dataset using the defined partition
train_data <- cgen_pol[trainIndex, ,drop=FALSE]
tune_plus_val_data <- cgen_pol[-trainIndex, ,drop=FALSE]

# Define a new partition to split the remaining 25%
tune_plus_val_index <- createDataPartition(tune_plus_val_data$Building_limit,
                                           p = .6,
                                           list = FALSE,
                                           times = 1)

# Split the remaining ~25% of the data: 40% (tune) and 60% (val)
tune_data <- tune_plus_val_data[-tune_plus_val_index, ,drop=FALSE]
val_data <- tune_plus_val_data[tune_plus_val_index, ,drop=FALSE]

# Outcome of this section is that the data (100%) is split into:
# training (~75%)
# tuning (~10%)
# validation (~15%)




# Simple Model ------------------------------------------------------------

# GLM gamma
# manque + OCCUPPIED_UNIF car bcp missings
# mod <- glm(Building_limit ~ PRODUIT + SUPERFICIE_UNIF + PROD_NBETEXSS + PROD_SOUSOLON + OCCUPPIED_UNIF, data = train_data, family = Gamma(link = "identity"))
mod <- lm(log(Building_limit) ~ PRODUIT + SUPERFICIE_UNIF + PROD_NBETEXSS + PROD_SOUSOLON + OCCUPPIED_UNIF, data = train_data)
summary(mod)
# RsqGLM(mod = mod)

# mod.diag <- glm.diag(mod)
# glm.diag.plots(mod, mod.diag)

# Pred & eval
tune_data <- tune_data %>%
  mutate(pred = exp(predict(mod, tune_data)))

# tune_data <- tune_data %>% 
#   mutate(pred = predict(mod, tune_data))



########## Eval

summary(tune_data$pred)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#  1018590  1525441  1684212  1990303  2147987 13793772       14 

summary(tune_data$Building_limit)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 502000  1198150  1668500  2334533  2309000 19108000 

tune_data_melt <- melt(tune_data %>% select(MACTA_ID, Building_limit, pred), id.vars = "MACTA_ID")

tune_data_melt%>%
  ggplot(aes(x=value, fill=variable)) +
  geom_density(alpha=0.3)+ 
  scale_x_log10()+
  labs(x= "Reconstruction Cost")


tune_data %>% 
  ggplot(aes(x = pred, y = Building_limit)) +
  geom_point() +
  ggtitle("Condos & Appartments") +
  theme_bw()

tune_data %>% 
  filter(PRODUIT == "MCO") %>% 
  ggplot(aes(x = pred, y = Building_limit)) +
  geom_point() +
  ggtitle("Condos") +
  theme_bw()

tune_data %>% 
  filter(PRODUIT == "MPF") %>% 
  ggplot(aes(x = pred, y = Building_limit)) +
  geom_point() +
  ggtitle("Appartments") +
  theme_bw()




# Test écarts -------------------------------------------------------------

tune_data <- tune_data %>% 
  mutate(ecart = Building_limit - pred,
         ecart_prop = (Building_limit - pred) / Building_limit)




