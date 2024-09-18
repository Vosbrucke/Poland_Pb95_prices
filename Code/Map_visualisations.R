library(wesanderson)
library(ggtext)
library(tidyverse)
library(magrittr)
library(lubridate)
library(sf)
library(eurostat)
library(ggdraw)
library(rstudioapi)
library(viridis)
library(cowplot)
library(rmarkdown)


## Univariate maps

## Mapping annual increase in gasoline spending on univariate map
# Filter for CP07222 coicop category- PETROL to see change in spending
data <- get_eurostat("prc_hicp_manr", filters = list(coicop = "CP07222", lastTimePeriod = 2)) %>%
  filter(time == max(time)) %>%
  filter(!geo %in% c("EA18", "EA19", "EU27_2020", "EU28", "EEA")) %>%
  drop_na() %>%
  select(geo, values) %>% 
  rename(values_x = values) %>%
  filter(!geo %in% c("MT", "TR"))

legend <- "Change in % of expenditure"
title <- "Annual change in petrol expenditure in Europe"
subtitle <- "Change in inflation in the coicop petrol category, August 2022"
caption <- "Source: Eurostat"

univariate_map_eu(data = data, legend = legend, title = title, subtitle = subtitle, caption = caption)

ggsave("Plots/Annual change in gasoline spending in Europe.png", dpi = 900, height = 15, width = 22, units = "cm")


## Mapping mean gasoil prices a year ago on univariate map
fuel_price_EU <- read.csv("Processed_data/fuel_price_EU.csv", sep = ",")

data <- fuel_price_EU %>%
  dplyr::group_by(code) %>%
  dplyr::filter(row_number() == 52) %>%
  dplyr::rename(values_x = Euro_super_95, geo = code)

title <- "Petrol price in Europe"
subtitle <- paste("Weekly average price of Pb95 petrol per day", format(as.Date(data$date %>% unique()), "%d %B %Y"))
legend <- "Average price in €"
caption <- "Source: Eurostat"

univariate_map_eu(data = data[, c(1,3)], title = title, subtitle = subtitle, legend = legend, caption = caption)

ggsave("Plots/Gasoil prices in Europe on 2021-08-23.png", dpi = 900, height = 15, width = 22, units = "cm")


## Mapping mean current gasoil prices on univariate map
fuel_price_EU <- read.csv("Processed_data/fuel_price_EU.csv", sep = ",")

data <- fuel_price_EU %>%
  dplyr::group_by(code) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::rename(values_x = Euro_super_95, geo = code)

title <- "Petrol prices in Europe"
subtitle <- paste("Average weekly price of petrol Pb95 per day", format(as.Date(data$date %>% unique()), "%d %B %Y"))
legend <- "Average price in €"
caption <- "Source: Eurostat"

univariate_map_eu(data = data[, c(1,3)], title = title, subtitle = subtitle, legend = legend, caption = caption)

ggsave("Plots/Gasoil prices in Europe on 2022-08-29.png", dpi = 900, height = 15, width = 22, units = "cm")


## Mapping inflation on univariate map
data <- get_eurostat("prc_hicp_manr", filters = list(coicop = "CP00")) %>%
  # Filtering for latest available month with all the countries with data and removing Turkey as an outlier
  filter(time == max(time) - months(1), geo != "TR") %>% 
  rename(values_x = values) %>%
  drop_na() %>% 
  select(geo, values_x)

title <- "HICP inflation in Europe (August 2022)"
subtitle <- ""
legend <- "Inflation in %"
caption <- "Source: Eurostat"

univariate_map_eu(data = data, title = title, subtitle = subtitle, legend = legend, caption = caption)

ggsave("Plots/Inflation HICP in Europe.png", dpi = 900, height = 15, width = 22, units = "cm")


## Mapping share of YoY increase in energy and food prices in HICP inflation on univariate map
# Downloading inflation data
full_inflation <- get_eurostat("prc_hicp_manr") %>%
  dplyr::filter(time == max(time) - months(1)) %>%
  dplyr::filter(!geo %in% c("EA18", "EA19", "EU27_2020", "EU28", "EEA")) %>%
  drop_na() %>%
  dplyr::rename(inflation = values)

# Downloading weights data
full_weight <-  get_eurostat("prc_hicp_inw") %>%
  filter(time == max(time)) %>%
  filter(!geo %in% c("EA18", "EA19", "EU27_2020", "EU28", "EEA")) %>%
  drop_na() %>%
  rename(weight = values) %>%
  mutate(weight = weight / 10)

# Joining two data frames
all_inflation <- full_inflation %>%
  inner_join(full_weight, by = c("coicop", "geo"))

# Reading special aggregates used by Eurostat to inspect inflation
special_aggregates <- read.csv(url('https://raw.githubusercontent.com/Vosbrucke/Poland_Pb95_prices/main/Processed_data/special_aggregates.csv'), sep = ",")

# Filtering aggregates to only some that are most interesting (for future analysis; not used in this code)
special_aggregates_int <- special_aggregates[c(1, 7, 8, 12, 27, 33, 34, 36), 1]

# Selecting 'Overall index excluding energy, food, alcohol and tobacco'
special_aggregates_selected <- special_aggregates[21, 1]

# Adjusting data frame by filtering for Turkey as an outlier and keeping only selected aggregate observations
inflation <- all_inflation %>% 
  filter(geo != "TR") %>%
  mutate(growth_total = weight * inflation / 100) %>% 
  filter(coicop == special_aggregates_selected) %>%
  arrange(growth_total) %>%
  group_by(geo)

# Joining inflation with weight and inflation with selected aggregate observations to 
# calculate share of YoY increase in energy and food prices on HICP inflation
joined_inflation <- full_inflation %>% 
  filter(coicop == "CP00") %>% 
  inner_join(inflation, by = "geo") %>% 
  mutate(perc_of_inflation = (inflation.x - growth_total) / inflation.x * 100)

# Change the structure to fit to map function
inflation <- joined_inflation %>% 
  select(geo, perc_of_inflation) %>% 
  rename(values_x = perc_of_inflation)

univariate_map_eu(data = inflation, subtitle = "", title = "The impact of year-on-year growth in energy and food prices on HICP inflation, July 2022", legend = "Share in %", caption = "Source: Eurostat")

ggsave("Plots/Share of YoY increase in energy and food prices on HICP inflation.png", dpi = 900, height = 15, width = 22, units = "cm")


## Bivariate maps

## Mapping inflation and share of YoY increase in energy and food prices in HICP inflation on bivariate map
data_y <- get_eurostat("prc_hicp_manr", filters = list(coicop = "TOT_X_NRG_FOOD")) %>%
  dplyr::filter(time == max(time) - months(1)) %>%
  dplyr::filter(!geo %in% c("EA18", "EA19", "EU27_2020", "EU28", "EEA")) %>%
  drop_na() %>%
  dplyr::rename(values_y = values) %>% 
  select(geo, values_y)

data_x <- get_eurostat("prc_hicp_manr", filters = list(coicop = "CP00")) %>%
  dplyr::filter(time == max(time) - months(1)) %>%
  dplyr::filter(!geo %in% c("EA18", "EA19", "EU27_2020", "EU28", "EEA")) %>%
  drop_na() %>%
  dplyr::rename(values_x = values) %>% 
  select(geo, values_x)

data_y <- data_y %>% 
  inner_join(data_x, by = "geo") %>% 
  mutate(values_y = (values_x - values_y) / values_x * 100) %>% 
  select(-values_x)

legend_x <- "lower → higher\ninflation"
legend_y <- "Share of energy resources in inflation\nlower → higher"
title <- "Inflation level and share of energy price increases in Europe"
caption <- "Source: Eurostat"

bivariate_map_eu(data_x = data_x, data_y = data_y, title = title, legend_x = legend_x, legend_y = legend_y, caption = caption, annotation = FALSE)

# If you need to add annotations change annotation to TRUE in a map function. The plot is then saved to global environment and you can operate on top of it. I recommend using ggannotate package.
# ggannotate::ggannotate(bivariate_plot)
ggsave("Plots/Inflation and share of YoY increase in energy and food prices in HICP inflation in Europe.png", dpi = 900, height = 15, width = 22, units = "cm")


## Mapping inflation and base inflation on bivariate map
# Downloading and adjusting data from eurostat
inflation <- get_eurostat(id = "prc_hicp_manr",
                          filters = list(coicop = c("CP00", "TOT_X_NRG_FOOD", "FOOD", "NRG", "IGD_NNRG", "SERV"))) %>% 
  filter(!geo %in% c("EU28", "EU27_2020", "EA18", "UK", "US", "EEA")) %>% 
  filter(time == max(time) - months(1) & !is.na(values)) %>%
  mutate(year = lubridate::year(time))

# Read english country names to plot
countries_eu <- read.csv(url("https://raw.githubusercontent.com/Vosbrucke/Poland_Pb95_prices/main/Processed_data/countries_eu_inflation.csv"), sep = ",")[,c(1,3)]

# Filter inflation by blocked region
inflation <- inflation %>% 
  left_join(countries_eu, by = c("geo" = "code")) %>% 
  filter(geo != "TR")

# Make a data frame for base inflation and inflation
inflation <- inflation %>%
  # Filter for series of the line plots
  filter(coicop %in% c("CP00", "TOT_X_NRG_FOOD"),
         !is.na(values)) %>%
  # Last formatting for graph
  mutate(line = factor(coicop, levels = c("CP00", "TOT_X_NRG_FOOD"),
                       labels = c("HICP Inflation", "Core Inflation (excluding energy and food)")))

# Make a data frame for inflation
data_x <- inflation %>%
  filter(coicop == "CP00") %>% 
  select(geo, values) %>% 
  rename(values_x = values)

# Make a data frame for base inflation
data_y <- inflation %>% 
  filter(coicop == "TOT_X_NRG_FOOD") %>% 
  select(geo, values) %>% 
  rename(values_y = values)

legend_x <- "lower → higher\ninflation"
legend_y <- "core inflation\nlower → higher"
title = "Inflation and core inflation (excluding energy and food) in Europe"
caption <- "Source: Eurostat"

bivariate_map_eu(data_x = data_x, data_y = data_y, legend_x = legend_x, legend_y = legend_y, title = title, caption = caption)

ggsave("Plots/Inflation and base inflation in Europe.png", dpi = 900, height = 15, width = 22, units = "cm")


## Mapping inflation and purchasing power adjusted GDP per capita on bivariate map
data_x <- get_eurostat("prc_hicp_manr", filters = list(coicop = "CP00")) %>%
  dplyr::filter(time == max(time) - months(1)) %>%
  dplyr::filter(!geo %in% c("EA18", "EA19", "EU27_2020", "EU28", "EEA")) %>%
  drop_na() %>%
  select(geo, values) %>% 
  dplyr::rename(values_x = values)

data_y <- get_eurostat("sdg_10_10", filters = list(na_item = "EXP_PPS_EU27_2020_HAB", lastTimePeriod = 1)) %>% 
  semi_join(data_x["geo"], by = "geo") %>% 
  select(geo, values) %>% 
  rename(values_y = values)

data_x <- data_x %>% 
  semi_join(data_y["geo"], by = "geo")

legend_x <- "lower → higher\ninflation"
legend_y <- "Purchasing power adjusted GDP per capita\nlower → higher"
title <- "Inflation level and purchasing power adjusted GDP per capita in Europe"
caption <- "Source: Eurostat"

bivariate_map_eu(data_x = data_x, data_y = data_y, legend_x = legend_x, legend_y = legend_y, title = title, caption = caption)

ggsave("Plots/Inflation and purchasing power adjusted GDP per capita in Europe.png", dpi = 900, height = 15, width = 22, units = "cm")

