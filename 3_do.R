# analysis
library(tidyverse)
library(tidylog)

county <- "01073"
peers <- c("01073","18097","22033","22071","29095",
                  "36029","36055","39035","47157","51710",
                  "51760","55079","21111","47037","01089")

# county ======
EXIM_merged %>%
  filter(!is.na(stco_code))%>%
  group_by(stco_code, stco_name, co_emp)%>%
  summarise_at(vars(contains("amt_")), sum)%>%
  mutate(amt_per_emp = amt_tot/co_emp,
         pct_small = amt_small/amt_tot, 
         pct_woman = amt_woman/amt_tot,
         pct_minority = amt_minority/amt_tot) %>%
  filter(stco_code %in% peers)

# tract =======
load("data/wac_FDIC_CDFI.rda")

master <- wac_FDIC_CDFI %>%
  filter(year >= 2014) %>%
  mutate(stco_code = substr(stcotr_code, 1,5)) %>%
  mutate(amt_tot_CDFI = ifelse(is.na(amt_tot_CDFI), 0, amt_tot_CDFI))%>%
  left_join(metro.data::county_cbsa_st[c("stco_code","stco_name" ,"cbsa_code", "cbsa_name")], by = "stco_code")

# tract summary ======

demo_summary <- function(df){
  df %>%
    summarise(amt_per_black = weighted.mean(amt_per_emp, emp_black_17, na.rm = T),
              # amt_per_black2 = sum(amt_per_emp * emp_black_17)/sum(emp_black_17),
              amt_per_white = weighted.mean(amt_per_emp, emp_white_17, na.rm = T), 
              amt_per_asian = weighted.mean(amt_per_emp, emp_asian_17, na.rm = T), 
              amt_per_latino = weighted.mean(amt_per_emp, emp_latino_17, na.rm = T), 
              amt_per_other = weighted.mean(amt_per_emp, emp_other_17, na.rm = T),
              
              amt_per_female = weighted.mean(amt_per_emp, emp_female_17, na.rm = T),
              amt_per_male = weighted.mean(amt_per_emp, emp_tot_17 - emp_female_17, na.rm = T),
              
              amt_per_below40k = weighted.mean(amt_per_emp, emp_below40k_17, na.rm = T),
              amt_per_tradable = weighted.mean(amt_per_emp, emp_tradable_17, na.rm = T),
              
              amt_per_total = weighted.mean(amt_per_emp, emp_tot_17, na.rm = T)) 
}

summary <- master %>%
  mutate(amt_per_emp = amt_tot/emp_tot_17) %>%
  group_by(stco_code, stco_name) %>%
  demo_summary()

summary <- master %>%
  mutate(amt_per_emp = amt_tot_CDFI/emp_tot_17) %>%
  group_by(stco_code, stco_name) %>%
  demo_summary()

# visualize ===========
gg_summary <- summary %>%
  pivot_longer(amt_per_black:amt_per_total, names_to = "group", values_to = "amt_per_emp")%>%
  mutate(cat = case_when(
    grepl("white|black|asian|latino|other", group) ~ "race",
    grepl("male", group) ~ "gender",
    T ~ "other"
  )) %>%
  mutate(group = gsub("amt_per_", "", group))

ggplot(gg_summary %>% filter(stco_code == county), 
       aes(x = reorder(group,as.numeric(as.factor(cat))), y = amt_per_emp, fill = cat))+
  geom_col()+
  geom_text(aes(label = scales::dollar(amt_per_emp, accuracy = 0.1)))+
  # facet_grid(rows = vars(year), cols = vars(cat), scales = "free")+
  NULL


summary %>%
  filter(stco_code %in% peers)
