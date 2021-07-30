
#repeated measures -- weighted proportion of people with caseness on GAD-7 
#(10 or more) and then confidence intervals 
install.packages("dplyr")

library(dplyr)
install.packages("dplyr")
library(srvyr)
install.packages("srvyr")
library(survey)
library(tidyverse)
library(here)
install.packages("stringr")
library(stringr)


fu <- filter(sel, dap == 53)
des <- svydesign(id = ~pid, weights = ~w_comb, data = sel)
odp <- function(x) {sprintf("%.1f", x) }
sprintf("%.1f", 22.222)
round(22.222, 1)

#KEY 

des_rep <- sel %>%
  as_survey_design(id = pid, weights = w_comb)

des_rep %>%
  mutate(gad_case = gad_total >= 10) %>%
  group_by(dap, gad_case) %>%
  summarise(prop = survey_prop("ci")) %>%
  mutate(cell = str_glue("{odp(prop)} [{odp(prop_low)}, {odp(prop_upp)}]"))  %>%
  select(dap, gad_case, cell) %>%
  spread(gad_case, cell) %>% 
  select(`TRUE`) %>%
  print(n=100)

##################################################################
#############################################################################

#Counts for demographics and weighted proportion of demographic groups for table 1 

##Distinct data frame sel with distinct rows  
nrow(distinct(sel, pid))
nrow(distinct_sel)

distinct_sel <- distinct(sel, pid, .keep_all = TRUE)


#Counts & w_comb proportions for gender 
gender <- sel %>% 
  filter(dap == 0)%>%
  select(pid, bl_female = female)

gender %>%
  group_by(bl_female) %>%  
  count(bl_female)

distinct_sel %>%
  group_by(female) %>%
  count(female)

des_rep <- sel %>%
  as_survey_design(id = pid, weights = w_comb)

des_rep %>%
  group_by(female) %>%
  summarise(prop = survey_prop("ci")) %>%
  mutate(cell = str_glue("{odp(prop)} [{odp(prop_low)}, {odp(prop_upp)}]")) 

#Counts & weighted proportions for is_staff 

role <- sel %>% 
  filter(dap == 0)%>%
  select(pid, bl_staff = is_staff)

role %>%
  group_by(bl_staff) %>%  
  count(bl_staff)

des_rep <- sel %>%
  as_survey_design(id = pid, weights = w_comb)

des_rep %>%
  group_by(is_staff) %>%
  summarise(prop = survey_prop("ci")) %>%
  mutate(cell = str_glue("{odp(prop)} [{odp(prop_low)}, {odp(prop_upp)}]")) 

#Count & proportions for prev_depress 
depress <- sel %>% 
  filter(dap == 0)%>%
  select(pid, bl_depress = prev_depress)

depress %>%
  group_by(bl_depress) %>%  
  count(bl_depress)

distinct_sel %>%
  group_by(prev_depress) %>%
  count(prev_depress)


des_rep <- sel %>%
  as_survey_design(id = pid, weights = w_comb)

des_rep %>%
  group_by(prev_depress) %>%
  summarise(prop = survey_prop("ci")) %>%
  mutate(cell = str_glue("{odp(prop)} [{odp(prop_low)}, {odp(prop_upp)}]")) 

#Counts & weighted proportions for prev_gad 
gad <- sel %>% 
  filter(dap == 0)%>%
  select(pid, bl_gad = prev_gad)

gad %>%
  group_by(bl_gad) %>%  
  count(bl_gad)

distinct_sel %>%
  group_by(prev_gad) %>%
  count(prev_gad)


des_rep <- sel %>%
  as_survey_design(id = pid, weights = w_comb)

des_rep %>%
  group_by(prev_gad) %>%
  summarise(prop = survey_prop("ci")) %>%
  mutate(cell = str_glue("{odp(prop)} [{odp(prop_low)}, {odp(prop_upp)}]"))


#Counts & weighted proportions for ethnicity 
ethn <- sel %>% 
  filter(dap == 0)%>%
  select(pid, bl_eth = ethnic_f)

ethn %>%
  group_by(bl_eth) %>%  
  count(bl_eth)

distinct_sel %>%
  group_by(ethnic_f) %>%
  count(ethnic_f)


des_rep <- sel %>%
  as_survey_design(id = pid, weights = w_comb)

des_rep %>%
  group_by(ethnic_f) %>%
  summarise(prop = survey_prop("ci")) %>%
  mutate(cell = str_glue("{odp(prop)} [{odp(prop_low)}, {odp(prop_upp)}]"))


#Counts & weighted proportions for age  
agec <- sel %>% 
  filter(dap == 0)%>%
  select(pid, bl_age = agecat)

agec %>%
  group_by(bl_age) %>%  
  count(bl_age)

distinct_sel %>%
  group_by(agecat) %>%
  count(agecat)

des_rep <- sel %>%
  as_survey_design(id = pid, weights = w_comb)

des_rep %>%
  group_by(agecat) %>%
  summarise(prop = survey_prop("ci")) %>%
  mutate(cell = str_glue("{odp(prop)} [{odp(prop_low)}, {odp(prop_upp)}]"))

#Counts & weighted proportions for household members 
livealone <- sel %>% 
  filter(dap == 0)%>%
  select(pid, bl_livealone = livalon)

livealone %>%
  group_by(bl_livealone) %>%  
  count(bl_livealone)

distinct_sel %>%
  group_by(livalon) %>%
  count(livalon)

des_rep <- sel %>%
  as_survey_design(id = pid, weights = w_comb)

des_rep %>%
  group_by(livalon) %>%
  summarise(prop = survey_prop("ci")) %>%
  mutate(cell = str_glue("{odp(prop)} [{odp(prop_low)}, {odp(prop_upp)}]"))

#Counts & weighted proportions for number of kids 
kids <- sel %>% 
  filter(dap == 0)%>%
  select(pid, bl_numchild = numchild)

kids %>%
  group_by(bl_numchild) %>%  
  count(bl_numchild)

distinct_sel %>%
  group_by(numchild) %>%
  count(numchild)

des_rep <- sel %>%
  as_survey_design(id = pid, weights = w_comb)

des_rep %>%
  group_by(numchild) %>%
  summarise(prop = survey_prop("ci")) %>%
  mutate(cell = str_glue("{odp(prop)} [{odp(prop_low)}, {odp(prop_upp)}]"))

#Counts & weighted proportions for participant is key worker
kw <- sel %>% 
  filter(dap == 0)%>%
  select(pid, bl_kwself = kwself_b)

kw %>%
  group_by(bl_kwself) %>%  
  count(bl_kwself)

distinct_sel %>%
  group_by(kwself_b) %>%
  count(kwself_b)

des_rep <- sel %>%
  as_survey_design(id = pid, weights = w_comb)

des_rep %>%
  group_by(kwself_b) %>%
  summarise(prop = survey_prop("ci")) %>%
  mutate(cell = str_glue("{odp(prop)} [{odp(prop_low)}, {odp(prop_upp)}]"))