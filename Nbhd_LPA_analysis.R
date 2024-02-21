# National residential typologies - LPA using US census tracts 
# Hiwot Zewdie 
# Last Modified: 01/2024
# Published: 02/2024

#environment settings
rm(list=ls())

library(tidycensus)
library(tidyr)
library(tidyLPA)
library(stringr)
library(withr)
library(MetBrewer)
library(tidyverse)


# Step 1: Pull select variables for all census tracts in the US 
ustracts <- NULL

for(i in state.abb){
  x <- get_acs(geography = "tract", 
               state = i, 
               geometry = FALSE, 
               year = 2019, 
               variables = c("B01001_001","B08006_001","B15003_001","B05012_002",
                             "B02001_002","B08006_002","B15003_017","B15003_022",
                             "B15003_023","B15003_025","B15003_024", "B25024_001", "B25024_002", 
                             "B08006_016", "B08006_017"))
  ustracts <- rbind(ustracts, x)
}


#Clean output and derive proportions from ACS counts 
ustracts_clean <- ustracts%>%
  dplyr::select(., -moe)%>%
  pivot_wider(.,names_from = variable, values_from = estimate)%>%
  rename(total_pop = B01001_001,
         workers16older = B08006_001,
         pop25older = B15003_001,
         native_born = B05012_002,
         white_alone = B02001_002,
         travel_car = B08006_002,
         travel_motorbike = B08006_016,
         travel_wfh = B08006_017,
         ed_hs = B15003_017,
         ed_bachelors = B15003_022, 
         ed_masters = B15003_023,
         ed_prof = B15003_024,
         ed_doc = B15003_025,
         struc_total = B25024_001,
         struc_single_unit_detached = B25024_002)%>% 
  mutate(ed_total = ed_hs+ed_bachelors+ed_masters+ed_prof+ed_doc,
         prop_native = native_born/total_pop,
         prop_white = white_alone/total_pop,
         prop_travel_car = (travel_car+travel_motorbike+travel_wfh)/workers16older,
         prop_educ = ed_total/pop25older,
         prop_single_unit_detached = struc_single_unit_detached/struc_total)%>%
  drop_na(prop_native,prop_white,prop_travel_car,prop_educ,prop_single_unit_detached)


# Step 2: Transform and standardize derived variables to prep for LPA 

ustracts_clean <- ustracts_clean %>%
  mutate(t_fborn = log(1-prop_native+0.01),
         t_remin = log(1-prop_white+0.01),
         t_travel_at = log(1-prop_travel_car+0.01),
         t_educ = prop_educ,
         t_single_unit_detached = prop_single_unit_detached)%>%
  drop_na(t_fborn,t_remin,z_travel_at,prop_educ,prop_single_unit_detached)


#make numeric
ustracts_lpa  <- ustracts_clean %>%
  mutate(z_fborn = as.numeric(scale(t_fborn)),
         z_remin = as.numeric(scale(t_remin)),
         z_travel_at = as.numeric(scale(z_travel_at)),
         z_educ = as.numeric(scale(prop_educ)),
         z_single_unit_detached = as.numeric(scale(prop_single_unit_detached)))%>%
  dplyr::select(z_fborn, 
                z_remin, 
                z_travel_at, 
                z_educ, 
                z_single_unit_detached)%>%
  as_tibble()


# Step 3: Run fit statistics and conduct LPA 

set.seed(36)

#compare profiles
compare_prof <- ustracts_lpa%>%
  estimate_profiles(n_profiles = 1:8, models = 6) %>%
  compare_solutions(statistics=c("AIC","BIC", "Entropy"))

#LPA
set.seed(36)

lpa_plot <- 
  ustracts_lpa%>%
  single_imputation() %>%
  estimate_profiles(n_profiles = 6, models = 6)

dff <- lpa_plot[["model_6_class_6"]][["dff"]]
estimates <- lpa_plot[["model_6_class_6"]][["estimates"]]


#  Step 4: Plot LPA results     

#pivot for plotting & change labels 
estimates$Variable <- " "
estimates$Variable[estimates$Parameter == "z_travel_at"] <- "Active transit" 
estimates$Variable[estimates$Parameter == "z_educ"] <- "Education \u2265 HS" 
estimates$Variable[estimates$Parameter == "z_single_unit_detached"] <- "Single-unit detached"  
estimates$Variable[estimates$Parameter == "z_fborn"] <- "Foreign born"  
estimates$Variable[estimates$Parameter == "z_remin"] <- "Racial ethnic minority"  

estimates$Variable <- factor(estimates$Variable, levels=c("Foreign born", "Racial ethnic minority",
                                                          "Active transit", "Education \u2265 HS", 
                                                          "Single-unit detached"))
#plotting estimated means

estimates%>%
  filter(Category == "Means")%>%
  ggplot(aes(x=Variable,y = Estimate,color = as.factor(Class), 
             linetype = as.factor(Class)))+
  geom_line(aes(group = Class))+ 
  geom_point()+
  xlab("Variable")+
  ylab("Mean Z-score")+
  labs(color = "Profile")+
  scale_linetype_manual(values = c(1,2,3,4,5,6), name="Profile") +
  theme_bw()+
  scale_color_manual(values = met.brewer("Lakota"))


#  Step 5: Format LPA results  

ustracts_clean <- mutate(ustracts_clean, ID = row_number())
dff <- mutate(dff, ID = row_number())

ustracts_lpa_table <- merge(ustracts_clean, dff)%>%
  dplyr::select(GEOID,NAME,Class, 
                starts_with("z"),
                starts_with("p"))%>%
  mutate_if(is.numeric, round, digits=3)%>%
  mutate(GEOID = as.numeric(GEOID))

col_order <- c("GEOID","NAME","Class","z_fborn", "prop_native", "z_remin", "prop_white",
               "z_travel_at", "prop_travel_car", "z_educ", "prop_educ", "z_single_unit_detached",
               "prop_single_unit_detached")

ustracts_lpa_table <- ustracts_lpa_table[, col_order]



