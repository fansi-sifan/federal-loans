# Author: Sifan Liu
# Date: Wed Jan 16 12:19:14 2019
# --------------

# SETUP ==================================================================
# REQUIRED PACKAGES ------------------------------------------
pkgs <- c('tidyverse', 'purrr')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 
# FUNC ------------------------------------------------------
# add leading zeros
padz <- function(x, n=max(nchar(x)))gsub(" ", "0", formatC(x, width=n)) 

# DEFINE GEO -------------------------------------------------
city_FIPS <- "07000"
ct_FIPS <- "073"
msa_FIPS <- "13820"
st_FIPS <- "01"
county_FIPS <- paste0(st_FIPS, ct_FIPS)
msa100_FIPS <- as.character((read.csv("V:/Sifan/R/xwalk/top100metros.csv") %>% filter(top100==1))[["GEO.id2"]])
# METRO PEERS -------------------------------
peerlist <- c("34980", "47260", "33340", "32820","40060", 
              "31140", "35380", "15380", "40380","26620",
              "28140", "17460", "26900", "12940", msa_FIPS)

msa_ct_FIPS <- read.csv('V:/Sifan/R/xwalk/county2msa.csv') %>%
  mutate(fips = paste0(padz(fipsstatecode,2), padz(fipscountycode,3)),
         COUNTY = trimws(toupper(gsub("County","",countycountyequivalent))))%>%
  filter(cbsacode %in% peerlist)%>%
  mutate(cbsa = as.character(cbsacode))%>%
  select(cbsa, FIPS = fips, metro = cbsatitle, county = COUNTY)

# LOAD DATA -------------------------------------------------
load("V:/Sifan/Birmingham/County Cluster/Temp data/SBA_loan_cleaned.Rda")


# ANALSIS =======================================================
# define the year ranges to group data --------------------------
year1 <- seq(2011,2016)
year2 <- seq(2005,2010)

# EXIM -----------------------------------------------------------
EXIM <- loan_datafiles$EXIM_matched%>%
  # only includes working capital loans, excludes insurance
  filter(Program =="Working Capital")  %>%
  mutate(
    year_range = case_when(
      Fiscal.Year %in% year2 ~ "2005 - 2010",
      Fiscal.Year %in% year1 ~ "2011 - 2016")) %>%
  group_by(county14,State, year_range)%>%
  summarise(amt.tot = sum(Approved.Declined.Amount, na.rm = TRUE),
            DISBamt.tot = sum(Disbursed.Shipped.Amount, na.rm = TRUE),
            count = n())%>%
  mutate(FIPS = padz(county14,5),
         program = "EXIM") %>%
  right_join(msa_ct_FIPS, by = "FIPS")
 
# SBA  -----------------------------------------------------------
SBA <- loan_datafiles$SBA_matched  %>%
  mutate(
    year_range = case_when(
      ApprovalFiscalYear %in% year2 ~ "2005 - 2010",
      ApprovalFiscalYear %in% year1 ~ "2011 - 2016")) %>%
  group_by(State, county14, year_range) %>%
  summarise(amt.tot = sum(GrossApproval, na.rm = TRUE))%>%
  mutate(FIPS = padz(as.integer(county14),5),
         program = "SBA") %>%
  right_join(msa_ct_FIPS, by = "FIPS")

# SSTR  -----------------------------------------------------------
SSTR <- loan_datafiles$SSTR_matched %>%
  mutate(
    year_range = case_when(
      Award.Year %in% year2 ~ "2005 - 2010",
      Award.Year %in% year1 ~ "2011 - 2016")) %>%
  group_by(State,county14, year_range) %>%
  summarise(amt.tot = sum(Award.Amount, na.rm = TRUE), 
            count = n())%>%
  mutate(FIPS = padz(as.integer(county14),5),
         program = "SSTR")%>%
  right_join(msa_ct_FIPS, by = "FIPS")


# TLR  -----------------------------------------------------------
TLR <- loan_datafiles$TLR_matched %>%
  mutate(
    year_range = case_when(
      Year %in% year2 ~ "2005 - 2010",
      Year %in% year1 ~ "2011 - 2016")) %>%
  # only includes investees that are businesses or CDFIs, excludes individuals
  filter(investeetype != "IND") %>%
  group_by(county14, year_range) %>%
  summarise(amt.tot = sum(originalamount, na.rm = TRUE),
            count = n()) %>%
  mutate(FIPS = padz(as.integer(county14),5),
         program = "CDFI") %>%
  inner_join(msa_ct_FIPS, by = "FIPS")

# FDIC  -----------------------------------------------------------
FDIC <- loan_datafiles$FDIC_matched %>%
  mutate(
    year_range = case_when(
      year %in% year2 ~ "2005 - 2010",
      year %in% year1 ~ "2011 - 2016")) %>%
  group_by(county14, year_range) %>%
  summarise_if(is.numeric,sum, na.rm = TRUE)%>%
  mutate(FIPS = padz(as.integer(county14),5),
         program = "FDIC",
         amt.tot = x_tot*1000) %>%
  inner_join(msa_ct_FIPS, by = "FIPS")

# COMBINED  -----------------------------------------------------------
PeerCounty_SMEloans <- bind_rows(
  sapply(list(SBA, EXIM,SSTR, TLR, FDIC), filter,FIPS%in%msa_ct_FIPS$FIPS))

PeerMetro_SMEloans <- bind_rows(
  lapply(list(SBA, EXIM,SSTR, TLR, FDIC), function(df){
    df %>% group_by(cbsa, metro, year_range, program)%>%
      summarise(amt.tot = sum(amt.tot, na.rm = TRUE))}))

# Project level details for MSA  --------------------------------------
MSA_SMEloan <- purrr::map(loan_datafiles,filter,county14 == as.numeric(county_FIPS))