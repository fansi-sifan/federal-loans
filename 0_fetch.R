# fetch latest data
library(tidyverse)

# FDIC ========================================================

# FDIC download, unzip and load all year data from FFIEC -----

download_FDIC <- function(year) {
  temp <- tempfile()
  download.file(paste0("https://www.ffiec.gov/cra/xls/", year, "exp_aggr.zip"), temp)
  
  # handle changes in file names
  con1 <- unz(temp, "exp_aggr.dat")
  con2 <- unz(temp, paste0(year, "exp_aggr.dat"))
  con3 <- unz(temp, paste0("cra20", year, "_Aggr_A11.dat"))
  data <- tryCatch(read_table(con1, col_names = FALSE),
                   error = function(e) {
                     tryCatch(read_table(con2, col_names = FALSE),
                              error = function(c) {
                                read_table(con3, col_names = FALSE)
                              }
                     )
                   }
  )
  # close connection, return data
  unlink(temp)
  return(data)
}

year_FDIC <- seq(10,18)
raw <- purrr::map_dfr(year_FDIC, download_FDIC)

temp <- raw %>% 
  unite(chr, X1:X2, remove = T, sep = "") %>%
  mutate(id = str_sub(chr, 1,4)) %>%
  filter(id == "A1-1") %>%
  # standardize all spaces to "A1-1 20xx"
  mutate(chr = str_replace(chr, "A1-1 ", "A1-1"),
         chr = str_replace(chr, "A1-1a", "A1-1"),
         chr = str_replace(chr, "A1-1", "A1-1 "))

# clean and merge -------------------------------------------
colnames <- c("ID","year", "type", "st_code", "co_code", "cbsa_code", "tract_code", "co_pop_cat", "tract_income_cat","level",
              "n_below100k", "amt_below100k", "n_100to250k", "amt_100to250k", "n_250kto1m", "amt_250kto1m", "n_stot", "amt_stot")

FDIC <- temp  %>%
  separate(chr, into = colnames, sep = c(5,9,11,13,16,21,28,30,33,36,46,56,66,76,86,96,106, 116)) %>%
  mutate_at(vars(matches("amt_|n_")), as.numeric) %>%
  mutate_at(c("year", "type", "level","tract_income_cat"), as.factor) %>%
  mutate(n_tot = n_below100k + n_100to250k + n_250kto1m,
         amt_tot = amt_below100k + amt_100to250k + amt_250kto1m,
         stco_code = paste0(st_code, co_code)) 


FDIC %>%
  # filter(level == "200") %>%   # couny
  # filter(level == "210") %>%   # msa
  # filter(level == "100") %>%   # income group
  # filter(level == "   ")%>%    # deal level
  filter(!is.na(n_tot))%>%
  count(year)

save(FDIC, file = "data/FDIC_10-18.rda")


# CDFI ======================
path <- "https://www.cdfifund.gov/Documents/FY%202017%20Data,%20Documentation,%20Instructions.zip"

# create a temporary directory
td = tempdir()
# create the placeholder file
tf = tempfile(tmpdir=td, fileext=".zip")
# download into the placeholder file
download.file(path, tf)

# get the name of the first file in the zip archive
fname = unzip(tf, list=TRUE)$Name

# unzip the file to the temporary directory
unzip(tf, files=fname, exdir=td, overwrite=TRUE)
purrr::walk(grep(".csv", fname , value = TRUE), function(x)unzip(tf, files = x, exdir = td,overwrite = T))

# read unzipped files
allfiles <- purrr::map_dfr(grep("TLR_", fname, value = T), function(x)read.csv(paste0(td,"///",x), colClasses = "character"))

CDFI <- allfiles %>%
  mutate(
    stcotr_code = ifelse(projectfipscode_2010 == "NONE", str_pad(projectfipscode_2000, 11, "left", "0"), 
                         str_pad(projectfipscode_2010, 11, "left", "0")),
    stco_code = substr(stcotr_code, 1, 5)
  ) %>%
  mutate(year = format(as.Date(dateclosed, "%d-%B-%y"), "%Y"))

save(CDFI, file = "data/CDFI.rda")

# SBA read raw ==============
SBA_7a <- metro.data::readxl_online("http://imedia.sba.gov/vd/general/foia/FOIA%20-%207(a)(FY2010-Present).xlsx")
SBA_504<- metro.data::readxl_online("http://imedia.sba.gov/vd/general/foia/FOIA%20-%20504%20(FY1991-Present).xlsx")

save(SBA_7a, file = "data/SBA_7a.rda")
save(SBA_504, file = "data/SBA_504.rda")

# EXIM raw from API =========

# install.packages("RSocrata")
library("RSocrata")

EXIM <- read.socrata(
  "https://data.exim.gov/resource/vbhv-d8am.json"
  # app_token = "YOURAPPTOKENHERE",
  # email     = "user@example.com",
  # password  = "fakepassword"
)

save(EXIM, file = "data/EXIM.rda")

# SBIR from API ============

data <- jsonlite::read_json("https://www.sbir.gov/api/awards.json?year=2018")

# SBIR ========================

library("httr")
library("readxl")
GET("https://query.data.world/s/h2fs7kwriotwuljmdf254hhll6nw6p", write_disk(tf <- tempfile(fileext = ".xlsx")))
SBIR <- read_excel(tf)%>% 
  # clean year 
  mutate(year = `Award Year`%>%as.numeric() %>% as_date(origin = "1900-01-01")%>% year())%>%
  mutate(year = case_when(
    grepl("January",`Award Year`) ~ as.numeric(str_sub(`Award Year`,-4,-1)),
    T ~ year
  )) 

save(SBIR, file = "data/SBIR.rda")
