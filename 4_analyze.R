library(tidyverse)
library(tidylog)

load("data/SBIR_cbsa.rda")
load("data/wac_FDIC_CDFI.rda")
load("data/SBA_cbsa.rda")

target_cbsa <- "13820"
target_co <- "01073"

# find insets=============
target_city <- "Birmingham"

y1 <- 2013
y2 <- 2018

target_cbsa <- "16980"
target_co <- "17031"
target_city <- "Chicago"

map <- tidycensus::get_acs(geography = "tract", variables = "B01003_001", county = str_sub(target_co, 3,5), state = str_sub(target_co,1,2), geometry = T)
city <- tigris::places(state = str_sub(target_co, 1,2),class = "sf") %>% tigris::filter_place(target_city)
# city <- sf::st_read("data/tl_2019_17_place") %>%
#   filter(NAME == "Chicago")

target_tract <- sf::st_intersection(map, city)

loan_master <- function(geo) {
  bind_rows(
    SBIR_cbsa %>%
      mutate(in_cbsa = cbsa_code == target_cbsa) %>%
      mutate(in_county = stco_code == target_co) %>%
      mutate(in_city = city_name == target_city) %>% 

      filter({{ geo }}) %>%
      filter(between(year, y1, y2)) %>%
      group_by(year) %>%
      summarise(
        total_SBIR = sum(`Award Amount`, na.rm = T),
        mean_SBIR = mean(`Award Amount`, na.rm = T)
      ) %>%
      pivot_longer(contains("_"))%>%
      separate(col="name", into = c("measure", "type")),


    SBA_cbsa %>%
      left_join(metro.data::county_cbsa_st[c("stco_code", "cbsa_code")], by = "stco_code") %>%
      mutate(year = as.integer(ApprovalFiscalYear)) %>%
      mutate(in_cbsa = cbsa_code == target_cbsa) %>%
      mutate(in_county = stco_code == target_co) %>%
      mutate(in_city = BorrCity == target_city) %>%

      filter({{ geo }}) %>%
      filter(between(year, y1, y2)) %>%
      group_by(Program, year) %>%
      summarise(
        total_SBA = sum(GrossApproval),
        mean_SBA = mean(GrossApproval)
      )%>%
      pivot_longer(contains("_"))%>%
      separate(col="name", into = c("measure", "type")),

    tmp %>%
      # filter(between(year, 2012, 2016)) %>%

      mutate(stco_code = str_sub(stcotr_code, 1, 5)) %>%
      left_join(metro.data::county_cbsa_st[c("stco_code", "cbsa_code")], by = "stco_code") %>%

      mutate(in_cbsa = cbsa_code == target_cbsa) %>% 
      mutate(in_county = stco_code == target_co) %>%
      mutate(in_city = stcotr_code %in% target_tract$GEOID) %>%

      filter({{ geo }}) %>%
      filter(between(year, y1, y2)) %>%
      group_by(year) %>%
      summarise(
        total_FDIC = sum(amt_tot, na.rm = T),
        nFDIC = sum(n_tot, na.rm = T),
        total_FDICsmall = sum(amt_stot, na.rm = T),
        nFDICsmall = sum(n_stot, na.rm = T),
        total_CDFI = sum(amt_tot_CDFI, na.rm = T),
        nCDFI = sum(n_tot_CDFI, na.rm = T)
      )%>%
      mutate(mean_FDIC = total_FDIC/nFDIC,
             mean_FDICsmall = total_FDICsmall/nFDICsmall,
             mean_CDFI = total_CDFI/nCDFI)%>%
      pivot_longer(contains("_"))%>%
      separate(col="name", into = c("measure", "type"))
  )
  
  
}


result <- bind_rows(
  loan_master(in_cbsa) %>% mutate(geo = "metro"),  
  loan_master(in_county) %>% mutate(geo = "county"),
  loan_master(in_city) %>% mutate(geo = "city")
) 

result %>%
  mutate(Program = ifelse(is.na(Program), type, paste(type, Program)))%>%
  # filter(measure == "tot")%>%
  # filter(Program!="FDIC")%>%
  ggplot(aes(x = year, y = value, color = geo, group = geo))+
  scale_y_continuous(labels = scales::dollar)+
  geom_point()+
  geom_line()+
  facet_wrap(Program~ measure, scales = "free_y",ncol = 2)+
  NULL

result %>% 
  select(-nFDIC, -nFDICsmall,-nCDFI)%>%
  pivot_wider(names_from = "year", values_from = "value") %>%
  mutate(CAGR_13_17 = (`2017`/`2013`)^(1/4)-1)%>%
  write.csv(paste0(target_city,"_finance.csv"))
