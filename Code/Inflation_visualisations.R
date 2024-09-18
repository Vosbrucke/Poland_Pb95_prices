library(wesanderson)
library(ggtext)
library(tidyverse)
library(magrittr)
library(lubridate)
library(patchwork)
library(glue)
library(ggh4x)
library(eurostat)


## The difference between the actual inflation and total inflation summed coicop categories
# Applying names to coicop categories
CPO_names <- c(
  "Food and non-alcoholic beverages",
  "Alcoholic beverages and tobacco",
  "Clothing and footwear",
  "Housing, water, electricity, gas and other fuels",
  "Furniture, household appliances and routine home maintenance",
  "Health",
  "Transportation",
  "Communication",
  "Recreation and culture",
  "Education",
  "Restaurants and hotels",
  "Miscellaneous goods and services"
)

# Generate CP01 to CP012 names to properly filter inflation coicop
one_to_twelve <- sapply(X = 1:12, FUN = function(X) {paste0("CP0", X)})

# Create a tibble with CP numbers and corresponding names
names <- tibble(one_to_twelve, CPO_names)

# Downloading inflation data for inflation categories
inflation <- get_eurostat("prc_hicp_manr", filter = list(coicop = c("CP00", one_to_twelve))) %>%
  filter(time == max(time) - months(1)) %>%
  filter(!geo %in% c("EA18", "EA19", "EU27_2020", "EU28", "EEA", "TR")) %>%
  drop_na() %>% 
  mutate(year = year(time))

# Downloading weights for inflation categories
weight_inflation <- get_eurostat("prc_hicp_inw", filter = list(coicop = c("CP00", one_to_twelve))) %>%
  filter(time == max(time)) %>%
  filter(!geo %in% c("EA18", "EA19", "EU27_2020", "EU28", "EEA")) %>%
  drop_na() %>% 
  mutate(year = year(time))

# Join two data frames and adjust
full_inflation <- inflation %>% 
  left_join(weight_inflation %>% select(-time), by = c("year", "geo", "coicop")) %>% 
  rename(inflation = values.x, weight = values.y) %>% 
  mutate(growth_total = weight / 1000 * inflation,
         perc_of_inflation = growth_total / inflation * 100)

# Checking whether inflation by coicop sums up to actual inflation
inflation <- data.frame(full_coicop = full_inflation %>% 
                          filter(coicop != "CP00") %>% 
                          mutate(coicop = factor(coicop, levels = c(one_to_twelve), labels = CPO_names)) %>%
                          arrange(growth_total) %>%
                          group_by(geo) %>% 
                          summarise(sum = sum(growth_total)),
                        actual = full_inflation %>%
                          filter(coicop == "CP00") %>% select(growth_total))

# Plot
inflation %>% 
  ggplot(aes(x = growth_total, y = full_coicop.sum, color = full_coicop.sum - growth_total)) +
  geom_abline() + 
  geom_point() +
  labs(x = "Eurostat's inflation", 
       y = "Calculated inflation using the main coicop categories", 
       title = "Difference between actual and weighted total inflation of coicop category",
       caption = "Source: Eurostat") +
  scale_color_viridis_c(option = "viridis", name = NULL, direction = -1) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 4),
        legend.position = "none")

ggsave("Plots/The difference between the actual inflation and total inflation summed coicop categories.png", dpi = 900, width = 20, height = 15, units = "cm")


## Checking whether weights by coicop sums up to 100
weight_inflation <- get_eurostat("prc_hicp_inw") %>%
  dplyr::filter(time == min(time)) %>%
  dplyr::filter(!geo %in% c("EA18", "EA19", "EU27_2020", "EU28", "EEA")) %>%
  drop_na()

# Plot
weight_inflation %>% 
  dplyr::filter(!geo %in% c("EA18", "EA19", "EU27_2020", "EU28", "EEA", "TR","EL")) %>%
  dplyr::filter(coicop %in% one_to_twelve) %>%
  group_by(geo) %>% 
  summarise(sum = sum(values) / 10) %>% 
  ggplot(aes(x = sum, y = sum - 100)) +
  geom_segment(aes(x = sum, xend = sum, y = sum - 100, yend = 0), color = "lightgrey", size = 0.2) +
  geom_point(aes(color = sum - 100)) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_gradient2(low = ("darkred"), mid = "black", high = ("darkred")) +
  labs(x = "Total weight of the main coicop categories",
       y = "Difference from the correct weight", 
       title = "The weights of the coicop categories should add up to 100",
       caption = "Source: Eurostat") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 4),
        legend.position = "none")

ggsave("Plots/The sum of the weights for the main coicop category weights.png", dpi = 900, width = 22, height = 15, units = "cm")


## Inflation components
# Preparing a data. Filtering for CP numbers, applying labels
inflation <- full_inflation %>% 
  dplyr::filter(geo != "TR") %>%
  mutate(growth_total = weight * inflation / 100,
         perc_of_inflation = growth_total / inflation * 100) %>%
  dplyr::filter(coicop %in% one_to_twelve) %>%
  mutate(coicop = factor(coicop, levels = c(one_to_twelve), labels = CPO_names)) %>%
  arrange(growth_total) %>%
  group_by(geo)

# Adjust data frame
inflation <- full_inflation %>% 
  dplyr::filter(coicop != "CP00") %>% 
  mutate(coicop = factor(coicop, levels = c(one_to_twelve), labels = CPO_names)) %>%
  arrange(growth_total) %>%
  group_by(geo)

# Create a tibble for a few interesting countries in the analysis. I am sticking with the ones I chose last time
inflation_highlight_letters <- tibble(geo = c("AT", "DE", "MT", "HU", "PL"), color = c("B", "D", "C", "E", "A"), names = c("Austria", "Germany", "Malta", "Hungary", "Poland")) %>%
  arrange(geo)

# Add countries letters to countries data
inflation_highlight <- inflation %>% right_join(inflation_highlight_letters, by = "geo")

country_code_name <- read.csv(url("https://raw.githubusercontent.com/Vosbrucke/Poland_Pb95_prices/main/Processed_data/countries_eu_inflation.csv"), sep = ",") %>% 
  # Select polish country names. There are english names available as well
  select(country_name_pl, code)

# Create a color palette
palette <- wesanderson::wes_palette("Darjeeling1", n = 5, type = "continuous")

# Final plot of an inflation by coicop categories from eurostat
inflation %>% 
  inner_join(country_code_name, by = c("geo" = "code")) %>% 
  ggplot() +
  geom_col(aes(x = country_name_pl, y = growth_total), fill = "black") +
  geom_col(data = inflation_highlight, aes(x = names, y = growth_total, fill = color)) +
  geom_hline(aes(yintercept = 0), color = "black", size = 0.5) +
  scale_fill_manual(values = palette, name = NULL) +
  labs(x = NULL, y = NULL, title = "Price increase in % by eurostat coicop category", caption = "Source: Eurostat") +
  scale_y_continuous(expand = c(0,0.2)) +
  facet_wrap(~ coicop, ncol = 1) +
  theme(axis.text = element_text(color = "black", size = 8),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey", size = 0.1),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 4))

ggsave("Plots/Inflation components.png", dpi = 900, width = 30, height = 30, units = "cm")


## Inflation function charts

inflation_chart(language_output = "en")

# For the plot with all regions height = 50 and width = 40 cm dimensions are recommended.
ggsave("Plots/Inflation in all countries.png", dpi = 900, height = 50, width = 40, units = "cm")


inflation_chart(region = c("Austria", "Malta", "Germany", "Poland", "Hungary"), language_output = "en")

ggsave("Plots/Inflation in selected countries.png", dpi = 900, height = 15, width = 25, units = "cm")
