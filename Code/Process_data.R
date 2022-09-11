library(tidyverse)
library(lubridate)
library(openxlsx)
library(rvest)

# Reading mean Pb 95 prices on gas stations in Poland
page <- readLines("https://www.bankier.pl/gospodarka/wskazniki-makroekonomiczne/eu-95-pol")

# Make points where the page contains important information
from <- which(page == page[str_detect(page, "// Create the chart")]) 
to  <- which(page == page[str_detect(page, "var intraday_dane")])[2] 

# Subset page
prices_url <-page[from:to][2]

# Split string on ',' and unlist
prices_url <- unlist(str_split(prices_url, pattern = ","))

# Make a sequence to further subset removing unnecesary data in this case between column which will be added later.
seq <- seq(1, length(prices_url), by = 2)

# Subset the data by sequence
prices_url <- prices_url[c(seq)]

# Subset the prices
prices_url_price <- prices_url[seq(1, length(prices_url),by = 2)]
prices_url_price <- gsub(".*:", "", prices_url_price)

# Subset the date
prices_url_date <- prices_url[seq(2, length(prices_url),by = 2)]
prices_url_date <- str_sub(prices_url_date, start = 9, end = 18)

# Combine in one table and add start and end column as well as year and week
prices_PB_95 <- data.frame(price =  prices_url_price, start = as.Date(prices_url_date) - lubridate::days(7), end = prices_url_date) %>% arrange(desc(start)) %>% mutate(year = year(end), week = week(end), price = as.numeric(price))

# Read USD/PLN and add year and week columns. Make weekly data
usdpln <- read.csv("https://stooq.com/q/d/l/?s=usdpln&i=d") %>% select(Date, Close) %>% mutate(year = year(Date), week = week(Date)) %>% group_by(year, week) %>% summarise(usdpln = mean(Close))

# Read Brent Oil prices and add year and week data columns. Make weekly data
brent <- gdata::read.xls("http://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls", sheet = 2, skip = 2) %>% 
  mutate(Date = mdy(Date)) %>% 
  rename(brent = last_col()) %>% 
  filter(Date > as.Date("2004-01-01")) %>% 
  mutate(week = week(Date), year = year(Date)) %>% 
  group_by(year, week) %>% 
  summarise(brent = mean(brent), Date) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(Date)) 

# Join Pb 95 prices with Brent Oil and USD/PLN
data <- prices_PB_95 %>% inner_join(brent,  by = c("year", "week")) %>% inner_join(usdpln, by = c("year", "week")) %>% mutate(brentPLN = brent * usdpln) %>% select(-c(usdpln, start, end))

# Read wholesale prices for Pb 95
ceny_hurtowe <- readLines("https://www.lotos.pl/145/type,oil_95/dla_biznesu/hurtowe_ceny_paliw/archiwum_cen_paliw")

# Make points where the page contains important information
from <- which(ceny_hurtowe == ceny_hurtowe[str_detect(ceny_hurtowe, "Opłata paliwowa")]) + 6
to  <- which(ceny_hurtowe == ceny_hurtowe[str_detect(ceny_hurtowe, "mainContent__footer")]) - 6

# Subset page
ceny_hurtowe <- ceny_hurtowe[from:to]

# Detect and remove 'td'
ceny_hurtowe <- ceny_hurtowe[str_detect(ceny_hurtowe, "td")] 

# Leave only numeric values
ceny_hurtowe <- gsub("[^0-9,-]", "", ceny_hurtowe)

# Change ',' to '.'
ceny_hurtowe <- gsub(",", ".", ceny_hurtowe)

# Make a function to assign data to correct column
function_seq <- function(data, i) {
  table <- data[seq(i, length(data), by = 4)]
}

# Run function
ceny_hurtowe <- data.frame(sapply(1:4, function_seq, data = ceny_hurtowe))

# Apply column names
colnames(ceny_hurtowe) <- c("Date", "price_hurt", "excise", "fuel_surcharge")

# Mutate Date column
ceny_hurtowe <- ceny_hurtowe %>% 
  mutate(Date = as.Date(Date), across(2:4, as.numeric))

# Transform data
ceny_hurtowe <- ceny_hurtowe %>% 
  mutate(across(2:4, ~ .x / 1000)) %>% 
  mutate(year =  year(Date), 
         week = week(Date)) %>% 
  group_by(year, week) %>% 
  mutate(across(2:4, mean), 
         price_hurt = price_hurt - excise - fuel_surcharge) %>% 
  filter(row_number() == 1)

# Information on how much observations there is from 2022-02-01 when VAT rate for Pb 95 gasoline was decreased from 23% to 8%
times <- nrow(data %>% filter(Date > as.Date("2022-02-01")))

# Information on how much observations there is from 2019-01-01 when emission tax was implemented
times_emission_tax <- nrow(data %>% filter(Date > as.Date("2019-01-01")))

# Create a share data
VAT_share <- c(rep(0.074, times), rep(0.187, nrow(data) - times))
store_margin_share <- c(rep(0.03, nrow(data)))
emission_tax <- c(rep(0.1, times_emission_tax), rep(0, nrow(data) - times_emission_tax))

# Bind share data
shares <- bind_cols(VAT_share, store_margin_share, emission_tax)

# Apply column names
colnames(shares) <- c("VAT_share", "store_margin_share", "emission_tax")

# Join data with wholesale prices
data <- data %>% inner_join(ceny_hurtowe, by = c("year", "week")) 

# Transform data
data <- data %>% 
  bind_cols(shares) %>% 
  mutate(emission_tax_share = emission_tax / price, 
         VAT = price * VAT_share, 
         store_margin = store_margin_share * price, 
         price_model = price_hurt + excise + fuel_surcharge + VAT + store_margin + emission_tax) %>% 
  select(-Date.y) %>% 
  rename(Date = Date.x)

# Order column names
data <- data %>% select(order(colnames(data)))

# Write csv
write_csv(data, "Processed_data/full_data.csv")


# Read bulletin data
oil_bulletin <- read.xlsx("https://ec.europa.eu/energy/observatory/reports/Oil_Bulletin_Prices_History.xlsx", sheet = "Prices with taxes, per CTR")

# Name columns
colnames(oil_bulletin) <- letters[1:8]

# Clean oil bulletin prices column for Pb95 
oil_bulletin_prices <- oil_bulletin$c[grep("[0-9]+$", oil_bulletin$c)] %>% gsub(",", "", .)

# Clean oil bulletin date column
date <- oil_bulletin$a[grep("[0-9]+$", oil_bulletin$a)]

# Correct date from integer to proper format
date <- as_date(as.numeric(date), origin = "1899-12-30")

# Delete NA's
date <- date[!is.na(date)]

# Drop NA's from oil bulletin
# oil_bulletin <- oil_bulletin$a %>% drop_na()

# Take countries from the oil bulletin data
countries_vector <- oil_bulletin$a[grep("[A-Z]", oil_bulletin$a)]

# Delete observation that are not countries names
countries_vector <- countries_vector[str_length(countries_vector) == 2]

# Check number of dates- there should be equal number to later add countries code to proper date points
count <- data.frame(date) %>%
  table

# Make a data frame 
count <- data.frame(
  char = names(count), 
  count = as.numeric(count), 
  stringsAsFactors=FALSE) 

# Filter for date that appears 27 times. Check what date it is
starting_date <- count %>%
  filter(count == 27) %>%
  head(n = 1) %>% 
  select(char) %>% 
  pull()

# Make a tibble with date and oil prices column. Leave observations below starting date
fuel_price <- tibble(date, Euro_super_95 = oil_bulletin_prices) %>% filter(date >= as.Date(starting_date))

# Make a vector with countries names
countries <- rep(countries_vector, length.out = nrow(fuel_price)) %>% sort()

# Make a final tibble. Change a oil price to reflect prices per 1 liter
fuel_price_EU <- tibble(code = countries, fuel_price) %>% 
  mutate(Euro_super_95 = round(as.numeric(Euro_super_95)) / 1000)

# Change a code for Greece to 'EL'. This will be important in plotting when other eurostat data writes Greece this way
fuel_price_EU$code[fuel_price_EU$code == "GR"] <- "EL"

# Write csv with fuel price
write_csv(fuel_price_EU, "Processed_data/fuel_price_EU.csv")

# Read csv
countries_all <- read.csv("Processed_data/slim-2_pl.csv", sep = ";", na.strings = "blank") %>% 
  select(-"country.code") %>% 
  rename(country_name = name, country_name_pl = name_pl, code = "alpha.2")


# Change Greece country code to be compatible with eurostat package
countries_all$code[countries_all$code == "GR"] <- "EL"

# Right join to code
countries_eu <- countries_all %>% 
  right_join(data.frame(code = possible_country_codes) %>% 
  filter(!row_number() %in% c(9, 10, 14)), by = "code")

# Make additional tibble for other regions
additional_countries_code <-  tibble(code = c("EA19", "EA", "EU"), country_name = c("Euro area (19 countries)", "Euro area", "European Union"), country_name_pl = c("Kraje strefy euro (19 krajów)", "Kraje strefy euro", "Unia Europejska"))

# Joing two data frames by binding rows
countries_eu <- countries_eu %>% 
  bind_rows(additional_countries_code) %>% 
  arrange(code)

# Write csv
write_csv(countries_eu, "Processed_data/countries_eu_inflation.csv") 

# Web scrape special aggregates for inflation categories
special_aggregates <- read_html("https://ec.europa.eu/eurostat/ramon/nomenclatures/index.cfm?TargetUrl=LST_NOM_DTL&StrNom=HICP_2000&StrLanguageCode=EN&IntPcKey=37598921&StrLayoutCode=HIERARCHIC") %>% 
  html_nodes(".text") %>% html_text2()

# Select only upper elements in the list- the actual aggregates
special_aggregates_u <- special_aggregates[
  grepl("[[:upper:]]+$", special_aggregates)]

# Adjust it
special_aggregates_u <- special_aggregates_u[-c(1:3)]

# Select only lower elements in the list- description of the aggregates
special_aggregates_l <- special_aggregates[
  grepl("[[:lower:]]", special_aggregates)]

# Adjust it and remove some left overs
special_aggregates_l <- special_aggregates_l[6:42] %>% stringr::str_remove("\n")

# Make a tibble
special_aggregates <- tibble(special_aggregates_u, special_aggregates_l)

# Write csv
write_csv(special_aggregates, "Processed_data/special_aggregates.csv")
