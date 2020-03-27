get_place <- function(df) {
  # get a list of unique places from the master file
  
  df %>%
    select(city_name, region_name) %>%
    mutate(
      pl_label = tolower(trimws(city_name)),
      st_name = trimws(region_name)
    ) %>%
    mutate(pl_label = ifelse(grepl("saint", pl_label),
                             gsub("saint", "st.", pl_label),
                             pl_label
    )) %>%
    unique()
}

match_place <- function(df) {
  # match the places using pl2co crosswalk
  load("../SifanLiu/data/pl2co.rda")
  
  place.matched <- df %>%
    left_join(pl2co[c("stpl_fips", "pl_label", "st_name", "stco_code", "afact1", "afact2")],
              by = c("pl_label", "st_name")
    ) %>%
    filter(!is.na(stco_code)) %>%
    mutate(afact1 = ifelse(is.na(afact1), 1, afact1),
           afact2 = ifelse(is.na(afact2), 1, afact2))%>%
    group_by(pl_label, st_name) %>%
    # assign place to county with highest afact1 and afact2
    top_n(afact1, n = 1) %>%
    top_n(afact2, n = 1) %>%
    ungroup() %>%
    select(city_name, region_name, pl_label, st_name, stco_code)
  
  place.unmatched <- dplyr::setdiff(df, place.matched %>% select(-stco_code))
  
  return(list(place.matched, place.unmatched))
}

match_cbsa <- function(df){
  
  unmatched = df[[2]]
  
  if (length(unmatched)>0) {
    print(paste0(nrow(unmatched), " places unmatched!"))
    print(unmatched)
    
  }
  
  matched = df[[1]] %>%
    left_join(metro.data::county_cbsa_st, by = "stco_code") %>%
    select(city_name, region_name, stco_code, cbsa_code, cbsa_name, cbsa_pop) %>%
    unique()
  
  return(matched)
}

states <- data.frame(state.abb,state.name) %>%
  add_row(state.abb = "DC", state.name = "District of Columbia")

counties <- metro.data::zcta2county %>%
  select(stco_code, stco_name)%>% 
  unique()%>%
  mutate(COUNTIES = toupper(stco_name))%>%
  mutate(COUNTIES = ifelse(grepl(" BOROUGH ", COUNTIES), 
                           gsub(" BOROUGH ", " ", COUNTIES),
                           COUNTIES))%>%
  mutate(COUNTIES = ifelse(grepl(" PARISH ", COUNTIES), 
                           gsub(" PARISH ", " ", COUNTIES),
                           COUNTIES)) %>%
  mutate(COUNTIES = if_else(COUNTIES == "BEDFORD VA (R)", "BEDFORD VA", COUNTIES))%>%
  mutate(COUNTIES = if_else(grepl("JOSEPH IN", COUNTIES), "ST JOSEPH IN", COUNTIES))

library(lubridate)

SBIR_xwalk <- SBIR %>%
  left_join(states, by = c("State" = "state.abb")) %>%
  rename(city_name = City, region_name = state.name)%>%
  get_place() %>%
  match_place()

SBIR_matched <- SBIR_xwalk %>%
  match_cbsa()

SBIR_cbsa <- SBIR %>%
  left_join(states, by = c("State" = "state.abb")) %>%
  rename(city_name = City, region_name = state.name)%>%
  left_join(SBIR_matched, by = c("city_name", "region_name"))

SBIR_cbsa %>%
  filter(year > 2014) %>%
  filter(is.na(stco_code)) %>%
  select(city_name, region_name)

save(SBIR_cbsa, file = "data/SBIR_cbsa.rda")

names(SBA_504)
names(SBA_7a)

SBA <- bind_rows(SBA_504,SBA_7a) %>%
  select(contains("Project"), contains("Borr"),ApprovalFiscalYear, NaicsCode, BusinessType, Program, GrossApproval)

# match county
matched_SBA <- SBA %>%
  mutate(COUNTY = trimws(toupper(ProjectCounty)),
         State = ProjectState)%>%
  select(COUNTY, State) %>%
  mutate(COUNTIES = paste(COUNTY, State)) %>%
  mutate(COUNTIES = ifelse(grepl("SAINT", COUNTIES),
                           gsub("SAINT", "ST.", COUNTIES),
                           COUNTIES))%>%
  distinct() %>%
  left_join(counties, by = "COUNTIES")

SBA_cbsa <- SBA %>%
  mutate(COUNTY = trimws(toupper(ProjectCounty)),
         State = ProjectState) %>%
  left_join(matched_SBA, by = c("COUNTY", "State"))
  
SBA_cbsa %>%
  filter(ApprovalFiscalYear>2014)%>%
  filter(is.na(stco_code)) %>%
  filter(State!="PR" & State!="GU")

save(SBA_cbsa, file = "data/SBA_cbsa.rda")
