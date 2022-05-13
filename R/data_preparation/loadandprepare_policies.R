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



# CGEN --------------------------------------------------------------------


# Extract inforce policies 
ext_inf_pc<- extract_policies(inforce = TRUE,
                              LOB= "COMM",
                              categ_business= "BIENS",
                              conversion= F,
                              additional_atgl = c(15183, 14356, 70, 71, 165, 169, 170),
                              preprocess= T) 


# Keep 2 last years for appartment buildings and condos 
policies_vig <- ext_inf_pc$after_preprocess$products %>% 
  filter(DT_VIG >= "2020-01-01", (PRODUIT == "MCO" | (PRODUIT == "MPF" & PROD_AFFECTAT %in% c("C6532", "C6542", "C6543", "C6531"))))


# Keep useful variables and appartment buildings built in the last 2 years or condos with professional evaluation
policies_sub <- policies_vig %>% 
  filter((PRODUIT == "MPF" & PROD_ANNEE >= "2020") | (PRODUIT == "MCO" & PROD_15183 == "O" & PROD_14356 %in% c("YES", "CONV"))) %>% 
  select(MACTA_ID, 
         PROD_15183,
         PROD_14356,
         PRODUIT,
         PROD_SUPERREZ,
         PROD_UMESSUPE,
         PROD_SUTOOCCO,
         PROD_UMESSUP2,
         PROD_NBETEXSS,
         PROD_SOUSOLON,
         PROD_CODEPOST,
         PROD_ANNEE,
         Building_limit,
         PROD_NOCIVIQ)


# qsave(policies_sub, here("data/input/cgen_policies.qs"))


# L'Unique ----------------------------------------------------------------

# Extract inforce policies 
ext_inf_pc_ugen <- extract_policies(inforce = TRUE,
                              company = "UGEN",
                              LOB= "COMM",
                              categ_business= "BIENS",
                              conversion= F,
                              additional_atgl = 15183,
                              preprocess= T) 

# qsave(ext_inf_pc_ugen, here("data/input/extract_ugen_policies.qs"))

pol_ugen <- ext_inf_pc_ugen$after_preprocess$situations
