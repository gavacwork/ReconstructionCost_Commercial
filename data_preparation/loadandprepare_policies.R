# Load and prepare data for reconstruction cost project
# By Gabrielle Vachon
# March 2022


library(extraw)
library(preprocess.ho)
library(extract.data)
library(dplyr)
library(here)
library(qs)


con <- init_con()


# Extract inforce policies 
ext_inf_pc<- extract_policies(inforce = TRUE,
                              LOB= "COMM",
                              categ_business= "BIENS",
                              conversion= F,
                              additional_atgl = 15183,
                              preprocess= T) 


# Keep 2 last years only 
policies_vig <- ext_inf_pc$after_preprocess$products %>% 
  filter(DT_VIG >= "2020-01-01")


