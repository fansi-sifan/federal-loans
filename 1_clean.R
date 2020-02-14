library(tidyverse)

# LOAD =====================
load("data/CDFI.rda")
load("data/FDIC_10-18.rda")
load("data/EXIM.rda")
load("data/SBA_504.rda")
load("data/SBA_7a.rda")
load("../../Birmingham/County Cluster/SSTR_cleaned.rda")
# load("../../Birmingham/County Cluster/SBIR_matched.rda")

# standardize =============

CDFI_cleaned <- CDFI %>%
  # only includes investees that are businesses, excludes individuals and CDFIs
  filter(investeetype == "BUS") %>%
  # only includes business related purpose (BUSFIXED, BUSINESS, BUSWORKCAP, MICRO, RECOCOM, RERHCOM)
  filter(purpose %in% c("BUSFIXED", "BUSINESS", "BUSWORKCAP", "MICRO", "RECOCOM", "RERHCOM")) %>%
  mutate(is.woman = case_when(
    gender %in% c("FEMALE", "BOTH") | womenownedorcontrolled == "YES" ~ TRUE,
    gender == "MALE" | womenownedorcontrolled == "NO" ~ FALSE,
    TRUE ~ NA
  ))%>%
  mutate(race = case_when(
    race == "WHITE" & hispanicorigin == "NO" ~ "white",
    race == "BLACK" & hispanicorigin == "NO" ~ "black",
    race == "ASIAN" & hispanicorigin == "NO" ~ "asian",
    race %in% c("ALASKAN", "PACIFIC", "HAWAIIAN") & hispanicorigin == "NO" ~ "other",
    race %in% c("", "OTHER") & hispanicorigin == "YES" ~ "latino",
    TRUE ~ "NA"
  )) %>%
  mutate(amt_tot = as.numeric(originalamount)) %>%
  mutate(year = as.numeric(year))%>%
  select(year, contains("code"), contains("id"),purpose, naicscode, amt_tot, is.woman, race)

CDFI_cleaned %>%
  group_by(year) %>%
  summarise(amt = sum(amt_tot, na.rm = T),
            mean = mean(amt_tot, na.rm = T)) %>%
  arrange(-year)

# FDIC  -----------------------------------------------------------
FDIC_cleaned <- FDIC %>%
  # convert thousand dollar to dollar
  mutate_at(vars(contains("amt_")), ~ .*1000, na.rm = TRUE) %>%
  select(year, level, contains("code"), contains("amt_"), contains("n_")) %>%
  # take out county/metro level summary
  filter(level == "   ")
  
FDIC_cleaned %>%
  group_by(year) %>%
  summarise(amt_tot = sum(amt_tot, na.rm = T))

# EXIM -----------------------------------------------------------

EXIM_cleaned <- EXIM %>%
  filter(program == "Working Capital") %>%
  filter(decision == "Approved") %>%
  mutate(amt_tot = as.numeric(approved_declined_amount), 
         amt_small = as.numeric(small_business_authorized_amount),
         amt_woman = as.numeric(woman_owned_authorized_amount),
         amt_minority = as.numeric(minority_owned_authorized_amount)) %>%
  mutate(year = as.numeric(fiscal_year)) %>%
  select(year, unique_identifier, naics4_code = primary_export_product_naics_sic_code, contains("primary"), contains("amt_"))
 
EXIM_cleaned %>%
  group_by(year)%>%
  summarise(amt_tot = sum(amt_tot))

summary(EXIM_cleaned$amt_tot)

# SBA  -----------------------------------------------------------

df <- purrr::map_dfr(list(SBA_504, SBA_7a),
                      ~ select(.,contains("Borr"), Program, ProjectCounty, ProjectState,
                               GrossApproval,ApprovalFiscalYear, NaicsCode, BusinessType))

SBA_cleaned <- df %>%
  mutate(
    year = as.numeric(ApprovalFiscalYear)
  ) %>%
  select(
    year, Program,
    amt_tot = GrossApproval,
    st_name = BorrState,
    st_name2 = ProjectState,
    ct_name = ProjectCounty,
    city_name = BorrCity,
    street_name = BorrStreet,
    zip_code = BorrZip,
    naics6_code = NaicsCode
  )

SBA_cleaned %>%
  group_by(year, Program) %>% 
  summarise(amt = sum(amt_tot,na.rm = T), 
            mean = mean(amt_tot, na.rm = T)) %>%
  arrange(-year)


# SSTR  -----------------------------------------------------------
SSTR_cleaned <- SSTR_blog %>%
  rename(stco_code = stco_fips, amt_tot = amt)


# SAVE OUTPUT ---------------------------------------------------
dfs <- objects()

datafiles <- mget(dfs[grep("_cleaned", dfs)])

# new <- mget(dfs[grep("export_ind", dfs)])
# datafiles <- gdata::update.list(datafiles, new)

save(datafiles, file = "data/loan_cleaned.rda")

