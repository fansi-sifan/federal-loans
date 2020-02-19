library(tidyverse)
library(tidylog)

# load datasets ===================
load("data/loan_cleaned.rda")
list2env(datafiles, .GlobalEnv)

# opportunity zones
load("../../metro.data/data/stcotr_oz.rda")

# LEHD, tract level employment, for FDIC & CDFI --------
lehd_path <- "../../../LEHD LODES/LEHD LODES v7.3/WAC_JT02/20"

clean_lehd <- function(yr){
#'  @param yr character, choose between "04" to "17"
  
  read_csv(paste0(lehd_path, yr, "_ALL_wac_JT02.csv")) %>%
    mutate(stcotr_code = str_sub(str_pad(w_geocode, 15, "left", "0"),1,11))%>%
    group_by(stcotr_code) %>%
    summarise_if(is.numeric, sum) %>%
    mutate(emp_tot = C000, 
           emp_below40k  = CE01 + CE02,
           emp_tradable = CNS01+CNS02+CNS05+CNS09+CNS10+CNS12+CNS13,
           emp_white = CR01, 
           emp_black = CR02, 
           emp_other = CR03 + CR05 + CR07, 
           emp_asian = CR04, 
           emp_latino = CT02, 
           emp_female = CS02, 
           emp_male = CS01) %>%
    select(stcotr_code, contains("emp")) %>%
    rename_at(vars(-one_of("stcotr_code")), ~ paste0(.,"_",yr))
}

# test <- clean_lehd("14")
# join multiple years
wac_emp <- full_join(clean_lehd(14), clean_lehd(17), by = "stcotr_code")


# merge ----------------------------------------

# tract level
FDIC_tomerge <- FDIC_cleaned %>%
  # keep only 50 states + DC
  filter(!(st_code %in% as.character(seq(60,95)))) %>%
  # merge on tract code
  mutate(stcotr_code = paste0(st_code, co_code, gsub("\\.","",tract_code)), 
         year = as.numeric(as.character(year))) %>%
  select(year, stcotr_code, contains("amt_"), contains("n_"))

CDFI_tomerge <- CDFI_cleaned %>%
  # better matches beyond 2014
  # filter(year >= 2014) %>%
  group_by(year, stcotr_code, purpose) %>%
  summarise(amt_tot_CDFI = sum(amt_tot, na.rm = T)) %>%
  pivot_wider(names_from = "purpose", values_from = "amt_tot_CDFI", names_prefix = "amt_") %>%
  ungroup() %>%
  # mutate row sum
  mutate(amt_tot_CDFI = rowSums(select(., contains("amt_")), na.rm = T))

wac_FDIC_CDFI <- wac_emp %>%
  left_join(full_join(FDIC_tomerge, CDFI_tomerge, by = c("year", "stcotr_code")), "stcotr_code") %>%
  left_join(stcotr_oz, by = "stcotr_code")

save(wac_FDIC_CDFI, file = "data/wac_FDIC_CDFI.rda")

# tmp <- wac_FDIC_CDFI %>%
#   select(contains("emp_"))%>%
#   select(contains("_17")) %>%
#   mutate(pct_nonwhite = (emp_tot_17 - emp_white_17)/emp_tot_17,
#          pct_female = emp_female_17/emp_tot_17, 
#          pct_poor = emp_below40k_17/emp_tot_17) 
# 
# skimr::skim(tmp)

# county level --------

load("../../Birmingham/County Cluster/Temp data/EXIM_matched.rda")

EXIM_merged <- EXIM_cleaned %>%
  rename(Unique.Identifier = unique_identifier) %>%
  left_join(EXIM_matched[c("Unique.Identifier", "county14")], by = "Unique.Identifier") %>%
  mutate(stco_code = str_pad(county14, 5,"left","0")) %>%
  group_by(stco_code, year) %>%
  summarise_at(vars(contains("amt_")), sum) %>%
  left_join(metro.data::county_cbsa_st[c("stco_code", "stco_name", "co_emp", "cbsa_code", "cbsa_name")]) 

save(EXIM_merged, file = "data/EXIM_merged.rda")

SSTR_merged <- SSTR_cleaned %>%
  ungroup() %>%
  group_by(stco_code, year) %>%
  summarise(amt_female = sum(amt_tot  *gender), 
            amt_minority = sum(amt_tot * disadv),
            amt_tot = sum(amt_tot)) %>%
  left_join(metro.data::county_cbsa_st[c("stco_code", "stco_name", "cbsa_code", "cbsa_name")]) 

save(SSTR_merged, file = "data/SSTR_merged.rda")
  