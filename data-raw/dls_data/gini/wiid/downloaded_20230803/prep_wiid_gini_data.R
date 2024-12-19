# prep UNU WIDER gini data

library(readxl)
library(here) # project root
library(dplyr)
library(readr)

filename_20230803 <- "wiidcountry_2.xlsx"
sheetname_20230803 <- "Sheet1"

wiid.data <- read_excel(
  path = here("data-raw","dls_data","gini","wiid","downloaded_20230803",filename_20230803),
  sheet = sheetname_20230803
)

wiid.gini <- wiid.data %>%
  select(c3,year,population,gini) %>%
  rename(iso=c3) %>%
  mutate(gini=gini/100)

write_delim(
  x = wiid.gini,
  file = here("data-raw","dls_data","gini","wiid","downloaded_20230803","wiid_gini.csv"),
  delim = ",")
