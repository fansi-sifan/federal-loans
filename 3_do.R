# analysis
library(tidyverse)
library(tidylog)

# find second largest
max2 <- function(x) max(x[x != max(x, na.rm = T)])

county <- "01073"

peers <- c(
  "01073", "18097", "22033",
  "22071", "21111", "47037",
  "29095", "36029", "36055",
  "39035", "47157", "51710",
  "51760", "55079", "01089"
)


# acs =========
load("../../metro-dataset/acs5_2018/co_acs_raw.rda")

acs_summary <- co_acs_raw %>%
  filter(stco_code %in% peers) %>%
  select(stco_code,
    baplus_white = S1501_C01_033E, baplus_black = S1501_C01_036E,
    baplus_asian = S1501_C01_042E, baplus_latino = S1501_C01_054E,

    hsplus_white = S1501_C01_032E, hsplus_black = S1501_C01_035E,
    hsplus_asian = S1501_C01_041E, hsplus_latino = S1501_C01_053E
  )

# sbo ==========
source("../../metro-dataset/census/SBO.R")

SBO_merged <- get_sbo_m(peers) %>%
  ungroup() %>%
  unique() %>%
  mutate(label = ifelse(label == "All firms classifiable by gender, ethnicity, race, and veteran status", "All_classified", label)) %>%
  filter(label != "Publicly held and other firms not classifiable by gender, ethnicity, race, and veteran status")

SBO_summary <- SBO_merged %>%
  select(-firmall) %>%
  pivot_wider(names_from = c("label", "is.traded"), values_from = "firmdemp") %>%
  mutate(
    stco_code = paste0(state, county),
    tot_firms = All_classified_traded + All_classified_local,
    pct_traded = All_classified_traded / tot_firms,

    pct_black_traded = `Black or African American_traded` / All_classified_traded,
    pct_black_all = (`Black or African American_traded` + `Black or African American_local`) / tot_firms,

    pct_white_traded = White_traded / All_classified_traded,
    pct_white_all = (White_traded + White_local) / tot_firms,

    pct_minority_traded = Minority_traded / All_classified_traded,
    pct_minority_all = (Minority_traded + Minority_local) / tot_firms
  ) %>%
  mutate(
    gap_traded = (max(pct_traded) - pct_traded) * tot_firms * pct_traded,
    gap_traded_avg = (weighted.mean(pct_traded, tot_firms) - pct_traded) * tot_firms
  ) %>%
  # mutate_at(vars(contains("pct")), .funs = list(gap = ~max(.)-.)) %>%
  rename_if(is.numeric, paste0, "_SBO") %>%
  select(stco_code, tot_firms_SBO, gap_traded_SBO, contains("pct"), everything())

SBO_summary$gap_traded_avg_SBO

# county ======

load("data/SSTR_merged.rda")

SSTR_summary <- SSTR_merged %>%
  filter(year >= 2014) %>%
  filter(stco_code %in% peers) %>%
  group_by(stco_code) %>%
  summarise_at(vars(contains("amt_")), ~ mean(.)) %>%
  mutate(
    pct_female = amt_female / amt_tot,
    pct_minority = amt_minority / amt_tot
  ) %>%
  rename_at(vars(contains("amt_")), paste0, "_SSTR")

load("data/EXIM_merged.rda")

EXIM_summary <- EXIM_merged %>%
  filter(year >= 2014) %>%
  filter(!is.na(stco_code)) %>%
  group_by(stco_code, stco_name, co_emp) %>%
  summarise_at(vars(contains("amt_")), mean) %>%
  mutate(
    amt_per_emp = amt_tot / co_emp,
    pct_small = amt_small / amt_tot,
    pct_woman = amt_woman / amt_tot,
    pct_minority = amt_minority / amt_tot
  ) %>%
  filter(stco_code %in% peers) %>%
  rename_at(vars(contains("amt_"), contains("pct_")), paste0, "_EXIM")

# tract =======
load("data/wac_FDIC_CDFI.rda")

master <- wac_FDIC_CDFI %>%
  filter(year >= 2014) %>%
  filter(!is.na(emp_tot_17)) %>%
  mutate(stco_code = substr(stcotr_code, 1, 5)) %>%
  filter(stco_code %in% peers) %>%
  mutate(amt_tot_CDFI = ifelse(is.na(amt_tot_CDFI), 0, amt_tot_CDFI)) %>%
  left_join(metro.data::county_cbsa_st[c("stco_code", "stco_name", "cbsa_code", "cbsa_name")], by = "stco_code")

# tract summary ======

demo_sum <- function(df, col, name) {
  col <- rlang::enquo(col)

  tmp <- df %>%
    mutate(amt_per_emp = !!col / emp_tot_17) %>%
    group_by(stco_code, stco_name)
  
  demo <- tmp  %>%

    # get 1) per capita loan, weighted by minority employment; 2) total
    summarise_at(vars(contains("_17")), list(
      ~ weighted.mean(amt_per_emp, ., na.rm = T),
      ~ sum(./5, na.rm = T)
    )) %>%
    rename_at(vars(contains("weighted.mean")), ~ gsub("emp_", "amt_per_", gsub("_17_weighted.mean", "", .))) %>%
    ungroup() %>%

    # calculate gap between best and average
    mutate(
      gap_amt_emp = (max(amt_per_tot) - amt_per_tot) * emp_tot_17_sum,
      gap_amt_emp_avg = (weighted.mean(amt_per_tot, emp_tot_17_sum) - amt_per_tot) * emp_tot_17_sum
    ) %>%
    rename_if(is.numeric, paste0, name)
  
  oz <- tmp %>%
    group_by(is.na(stco_type), add = T) %>%
    summarise(
      amt_tot = sum(amt_per_emp * emp_tot_17, na.rm = T),
      amt_per_emp = amt_tot / sum(emp_tot_17, na.rm = T)
    ) %>%
    pivot_wider(names_from = "is.na(stco_type)", values_from = c("amt_tot", "amt_per_emp")) %>%
    mutate(pct_amt_oz = amt_tot_FALSE / (amt_tot_TRUE + amt_tot_FALSE)) %>%
    select(stco_code, pct_amt_oz,
           amt_per_oz = amt_per_emp_FALSE,
           amt_per_noz = amt_per_emp_TRUE
    )
  
  bind_cols(demo, oz)
    
}

FDIC_summary <- master %>%
  demo_sum(amt_tot, "_FDIC")

CDFI_summary <- master %>%
  demo_sum(amt_tot_CDFI, "_CDFI")

gg_summary <- master %>%
  group_by(stco_code, stco_name) %>%
  summarise_at(vars(contains("amt_"), emp_tot_17), sum, na.rm = T) %>%
  ungroup() %>%
  mutate_at(vars(contains("amt_")), ~(./emp_tot_17))%>%
  pivot_longer(amt_below100k:amt_tot_CDFI, names_to = "program", values_to = "amt_per_emp") %>%
  mutate(category = case_when(
    grepl("0", program) ~ "FDIC", 
    grepl("[[:upper:]]", program) ~ "CDFI" ,
    T ~ "others"
  )) %>%
  mutate(category = ifelse(program %in% c("amt_tot", "amt_tot_CDFI"), "all", category))

library(RColorBrewer)
mycolor <- colorRampPalette(brewer.pal(12, "Set1"))(19)

g <- ggplot(gg_summary %>% 
         filter(category %in% c("CDFI","FDIC", "all")), 
         aes(x = stco_name, y = amt_per_emp, fill = program, label = scales::comma(amt_per_emp))) + 
  geom_col() + 
  scale_fill_manual(values = mycolor)+
  coord_flip() + 
  facet_wrap(~category,scales = "free_x") +
  theme_minimal()

g

plotly::ggplotly(g)

# --- oz
oz_summarize <- function(df) {
 
}



# SAVE OUTPUT ---------------------------------------------------

# merge summaries ----

all <- acs_summary %>%
  left_join(SBO_summary, by = "stco_code") %>%
  left_join(FDIC_summary, by = "stco_code") %>%
  left_join(CDFI_summary, by = c("stco_code", "stco_name")) %>%
  left_join(EXIM_summary, by = c("stco_code", "stco_name")) %>%
  left_join(SSTR_summary, by = "stco_code") %>%
  # left_join(metro.data::county_cbsa_st[c("stco_code", "co_emp")]) %>%
  select(stco_code, stco_name, emp_tot = emp_tot_FDIC, everything())

peergap_summary <- all %>%
  mutate(
    pct_owner = tot_firms_SBO / emp_tot,
    gap_allfirms_emp = (max(pct_owner) - pct_owner) * emp_tot,
    gap_allfirms_emp_avg = (weighted.mean(pct_owner, emp_tot) - pct_owner) * emp_tot,

    EXIM_per_traded = amt_tot_EXIM / All_classified_traded_SBO,
    gap_amt_traded_EXIM = (max(EXIM_per_traded, na.rm = T) - EXIM_per_traded) * All_classified_traded_SBO,
    gap_amt_traded_EXIM_avg = (weighted.mean(EXIM_per_traded, All_classified_traded_SBO, na.rm = T) - EXIM_per_traded) * All_classified_traded_SBO,

    SSTR_per_traded = amt_tot_SSTR / All_classified_traded_SBO,
    # Huntsville is an outlier
    gap_amt_traded_SSTR = (max2(SSTR_per_traded) - SSTR_per_traded) * All_classified_traded_SBO,
    gap_amt_traded_SSTR_ave = (weighted.mean(SSTR_per_traded, All_classified_traded_SBO, na.rm = T) - SSTR_per_traded) * All_classified_traded_SBO
  ) %>%
  select(stco_code, stco_name, emp_tot, contains("gap"),
    tot_firms_SBO,
    tot_traded = All_classified_traded_SBO, contains("amt_tot")
  )

demogap_summary <- all %>%
  mutate(
    rate_hsplus_all_black = tot_firms_SBO * pct_black_all_SBO / hsplus_black,
    rate_hsplus_all_white = tot_firms_SBO * pct_white_all_SBO / hsplus_white,

    rate_baplus_traded_black = `Black or African American_traded_SBO` / baplus_black,
    rate_baplus_traded_white = White_traded_SBO / baplus_white
  ) %>%
  transmute(
    stco_code = stco_code, stco_name = stco_name,
    gap_hsplus_all_white_black = (rate_hsplus_all_white - rate_hsplus_all_black) * hsplus_black,

    # tradable
    gap_baplus_traded_white_black = (rate_baplus_traded_white - rate_baplus_traded_black) * baplus_black,

    # finance, race
    gap_FDIC_white_black = amt_per_white_FDIC - amt_per_black_FDIC,
    gap_CDFI_white_black = amt_per_white_CDFI - amt_per_black_CDFI,
    gap_EXIM_white_nonwhite = amt_minority_EXIM / Minority_traded_SBO - (amt_tot_EXIM - amt_minority_EXIM) / Nonminority_traded_SBO,
    gap_SSTR_white_nonwhite = amt_minority_SSTR / Minority_traded_SBO - (amt_tot_SSTR - amt_minority_SSTR) / Nonminority_traded_SBO,

    # finance, gender
    gap_FDIC_male_female = amt_per_male_FDIC - amt_per_female_FDIC,
    gap_CDFI_male_female = amt_per_male_CDFI - amt_per_female_CDFI,
    gap_EXIM_male_female = amt_woman_EXIM / `Female-owned_traded_SBO` - (amt_tot_EXIM - amt_woman_EXIM) / `Male-owned_traded_SBO`,
    gap_SSTR_male_female = amt_female_SSTR / `Female-owned_traded_SBO` - (amt_tot_SSTR - amt_female_SSTR) / `Male-owned_traded_SBO`,

    # finance, oz
    gap_FDIC_oz = amt_per_noz_FDIC - amt_per_oz_FDIC,
    gap_CDFI_oz = amt_per_noz_CDFI - amt_per_oz_CDFI
  )

dfs <- objects()

loans <- mget(dfs[grep("_summary", dfs)])
names(loans)

openxlsx::write.xlsx(loans, file = paste0("result/", county, "_loan.xlsx"))
