library(wesanderson)
library(ggtext)
library(tidyverse)
library(magrittr)
library(lubridate)
library(patchwork)
library(glue)
library(ggh4x)


# Read data
data <- read_csv('Processed_data/full_data.csv')

## Model price in relation to the real price. 
# The differences result from the assumptions of the model in the form of a fixed margin of 
# the station and the market realities - adjustment to fluctuations does not take place immediately.

# The model price was calculated by summing up all the price factors for the Pb95 fuel.

# Create a palette
palette <- wesanderson::wes_palette("Zissou1", 7, type = "continuous")

# Plot
data %>% filter(Date > as.Date("2020-01-01")) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = price_model, color = "1"), size = 0.6, linetype = 5) +
  geom_line(aes(y = price, color = "2"), size = 0.6) +
  geom_line(aes(y = price_hurt, color = "3")) +
  geom_line(aes(y = excise, color = "4")) +
  geom_line(aes(y = VAT, color = "5")) +
  geom_line(aes(y = store_margin, color = "6")) +
  geom_line(aes(y = fuel_surcharge, color = "7")) +
  geom_line(aes(y = emission_tax, color = "8")) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0,9), 
                     breaks = seq(0,8,2)) +
  # scale_x_date(limits = c(as.Date("2019-11-30"), NA), 
  #              expand = c(0,0)) +
  scale_x_date(breaks = seq.Date(from = floor_date(as.Date("2020-01-01"), "month"), to = ceiling_date(max(data$Date), "month"), "6 months"), 
               date_labels = "%b %y",
               minor_breaks = seq.Date(from = floor_date(as.Date("2020-01-01"), "month"), to = ceiling_date(max(data$Date), "month"), "month"), 
               guide = "axis_minor",
               limits = c(floor_date(as.Date("2020-01-01"), "month"), ceiling_date(max(data$Date), "month")),
               expand = c(0, 0)) +
  scale_color_manual(name = NULL, 
                     values = c("#666666", palette), 
                     labels = c("Model price", "Retail price", "Wholesale price", "Emission tax", "VAT", "Fuel fee", "Excise", "Station's overhead")) +
  theme_classic() +
  labs(x = NULL, y = NULL, 
       title = "Prices of individual factors included in the retail price of Pb95", 
       subtitle = "The model price is the sum of the prices of the price-forming factors for Pb95",
       caption = "Source: Bankier, Lotos") +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(hjust = 0, size = 4),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text.x = element_text(color = "black", angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        text = element_text(color = "black"))

ggsave("Plots/Model vs actual price.png", dpi = 900, width = 20, height = 10, unit = "cm")


## Wholesale price vs brent price expressed in PLN

# Create a color palette
palette <- wesanderson::wes_palette("Zissou1", 2, type = "continuous")

# Filter data to start from the beginning of 2022. Add growth columns for brent and brent in PLN
data_growth <- data %>%  
  filter(Date > as.Date("2022-01-01")) %>% 
  mutate(last_price_hurt = 100 * price_hurt / last(price_hurt), 
         last_brentPLN = 100 * brentPLN / last(brentPLN))

# Limit data frame to latest observations
data_growth_last <- data_growth %>% 
  select(Date, last_price_hurt, last_brentPLN) %>% 
  filter(Date == max(Date))

# Bind rows of two data frames to make a line for a self-made legend 
data_growth_last_line <- bind_rows(data_growth_last, data_growth_last %>% 
                         mutate(Date = Date + days(15)))

# Plot
data_growth %>% 
  ggplot(aes(x = Date)) +
  geom_line(data = data_growth_last_line, aes(y = last_price_hurt, color = "Wholesale Price"), size = 0.3) +
  geom_text(data = data_growth_last_line, aes(x = Date[1] + days(18), y = last_price_hurt, label = "Wholesale Price", color = "Wholesale Price"), size = 3, hjust = 0) +
  geom_line(data = data_growth_last_line, aes(y = last_brentPLN, color = "Brent Crude Oil in PLN"), size = 0.3) +
  geom_text(data = data_growth_last_line, aes(x = Date[1] + days(18), y = last_brentPLN, label = "Brent Crude Oil in PLN", color = "Brent Crude Oil in PLN"), size = 3, hjust = 0) +
  geom_line(aes(y = last_price_hurt, color = "Wholesale Price"), size = 0.75) +
  geom_line(aes(y = last_brentPLN, color = "Brent Crude Oil in PLN"), size = 0.75) +
  geom_point(data = data_growth_last, mapping = aes(y = last_price_hurt, color = "Wholesale Price"), size = 1, shape = 21, fill = "white", stroke = 1) +
  geom_point(data = data_growth_last, mapping = aes(y = last_brentPLN, color = "Brent Crude Oil in PLN"), size = 1, shape = 21, fill = "white", stroke = 1) +
  scale_x_date(breaks = seq.Date(from = floor_date(min(data_growth$Date), "month"), to = ceiling_date(max(data_growth$Date), "month"), "3 months"), 
               date_labels = "%b %y",
               minor_breaks = seq.Date(from = floor_date(min(data_growth$Date), "month"), to = ceiling_date(max(data_growth$Date) + months(2), "month"), "month"), 
               guide = "axis_minor",
               limits = c(floor_date(min(data_growth$Date), "month"), ceiling_date(max(data_growth$Date) + months(2), "month")),
               expand = c(0, 0)
  ) +
  scale_color_manual(name = NULL, values = c("Wholesale Price" = palette[1], "Brent Crude Oil in PLN" = "#E4B80E")) +
  labs(x = NULL, y = NULL, 
       title = 'The wholesale price has "broken away" from Brent Oil quotes expressed in PLN', 
       subtitle = "100 = price level on 01/01/2022",
       caption = "Source: EIA, Lotos, Stooq") +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(hjust = 0, size = 4),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        text = element_text(color = "black"),
        axis.text.x = element_text(angle = 0),
        legend.position = "none")

ggsave("Plots/Wholesale vs brent price.png", dpi = 900, width = 20, height = 10, unit = "cm")


## Visualization of price factors for Pb95 fuel

# Data preparation
share_data <- data %>% 
  select(Date, price_hurt, excise, VAT, store_margin, emission_tax, fuel_surcharge)

# Make a pivot table
share_data  <- share_data %>% pivot_longer(cols = -Date, values_to = "price") %>% mutate(name = (factor(name, levels = "wholesale price", "excise", "VAT", "gas station's overhead", "fuel fee", "emission tax")))

# Create a color palette
palette <- wesanderson::wes_palette("Zissou1", 6, type = "continuous")

# Filter from the given date
share_data %<>% filter(Date > as.Date("2020-01-01"))

# Clearing data by replacing the date for the oldest observations with the correct one for visualization purposes
index <- which(share_data$Date == min(share_data$Date))

# Floor the oldest date. This is done to make a date label in the correct position
share_data$Date[index] <- floor_date(share_data$Date[index], unit = "month")

# Plot
old_names <- names(share_data)
new_names <- c("Date", "wholesale price", "excise", "VAT", "gas station's overhead", "fuel fee", "emission tax")
data.table::setnames(share_data, old_names, new_names)
share_data %>%
  pivot_longer(cols = -Date, values_to = "price") %>%
  ggplot(aes(fill = name, x = Date, y = price)) + 
  geom_bar(position = "fill", stat = "identity", width = 15) +
  geom_vline(xintercept = as.Date("2022-02-03"), color = "black") +
  # Add line to higlight a tax cut. The curve was added using ggannotate package
  geom_curve(data = data.frame(x = as.Date("2021-10-01"),
                               y = 0.747391474149104, xend = as.Date("2022-01-09"),
                               yend = 0.583470506718382),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
             curvature = 0.49, arrow = arrow(20L, unit(0.1, "inches"),
                                             "last", "closed"),
             inherit.aes = FALSE, color = "black") +
  # Add text to the line. The text was added using ggannotate package
  geom_text(data = data.frame(x = as.Date("2021-09-28"),
                              y = 0.818102479707454, label = "VAT reduction on fuel\nfrom 23% to 8%"),
            mapping = aes(x = x, y = y, label = label),
            inherit.aes = FALSE, color = "black") +
  scale_fill_manual(values = palette, 
                    labels = c("wholesale price", "excise", "VAT", "gas station's overhead", "fuel fee", "emission tax"), 
                    name = NULL) +
  scale_x_date(date_breaks = "6 months", 
               date_labels = "%b %y", 
               limits = c(floor_date(min(share_data$Date), "month"), max(share_data$Date))) + 
  coord_cartesian(expand = 0) +
  labs(y = NULL, x = NULL, 
       title = "Percentage share of individual price drivers for Pb95 fuel in Poland from 2020",
       caption = "Source: Bankier, e-Petrol, Lotos") +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(face = "bold", size = 9, hjust = 0.1),
        plot.caption = element_text(hjust = 0, size = 4),
        axis.line = element_blank(),
        axis.text.x = element_text(color = "black", angle = 90),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(color = "black"))

ggsave("Plots/Price-generating factors Pb95.png", dpi  = 900, width = 20, height = 10, unit = "cm")


# Choosing colors for brent oil in USD and PLN
palette <- wesanderson::wes_palette("Zissou1", 10, type = "continuous")

# 4th and 7th color chosen by associated with USD and PLN visual identification (green for USD, gold for PLN)
USD_color <- "#9EBE91"
PLN_color <- "#E4B80E"


## Brent in PLN

# Line plot
p1 <- data %>%
  filter(!year == 2004) %>%
  ggplot(aes(x = Date, y = brentPLN)) + 
  geom_line() +
  labs(x = NULL, y = NULL) +
  ggtitle("Brent Crude Oil in PLN 2005-2022") +
  scale_y_continuous(breaks = seq(0,600, 150), 
                     limits = c(0,650), 
                     expand = c(0,0)) +
  scale_x_date(date_labels = "%Y", 
               minor_breaks = seq.Date(as.Date("2005-01-01"), as.Date(now() + years(3)), by = "year"), 
               guide = "axis_minor") +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        text = element_text(color = "black"))

# Boxplot
p2 <- data %>%
  filter(!year == 2004) %>%
  ggplot(aes(x = year, y = brentPLN, group = year)) + 
  geom_boxplot() +
  labs(x = NULL, y = NULL,
       cation = "Source: EIA, stooq") +
  ggtitle("Brent Crude Oil in PLN 2005-2022") +
  scale_y_continuous(breaks = seq(0,600, 150), 
                     limits = c(0,650), 
                     expand = c(0,0)) +
  scale_x_continuous(minor_breaks = seq(2005, 2025, 1), 
                     guide = "axis_minor") +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(),
        plot.caption = element_text(hjust = 0, size = 4),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        text = element_text(color = "black"))

p_full <- p1 / p2

ggsave("Plots/Brent in PLN.png", p_full, dpi  = 900, width = 15, height = 20, unit = "cm")


## Range and median value for brent in PLN
data %>%
  filter(year != 2004) %>%
  group_by(year) %>% 
  mutate(median = median(brentPLN)) %>%
  ggplot(aes(x = year, y = brentPLN, group = year)) +
  geom_line() +
  geom_point(mapping = aes(y = median), shape = 22, color = USD_color, fill = "white", size = 2, stroke = 1) +
  labs(x = NULL, y = NULL,
       caption = "Source: EIA, stooq") +
  ggtitle("Range and <span style='color:#9EBE91;'>median</span> of Brent crude oil in PLN in 2005-2022") +
  scale_y_continuous(breaks = seq(0,600, 150), 
                     limits = c(0,650), 
                     expand = c(0,0)) +
  scale_x_continuous(minor_breaks = seq(2004, 2025, 1), 
                     guide = "axis_minor", 
                     limits = c(2004,(year(max(data$Date) + years(1)))), 
                     expand = c(0.,0)) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 4),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        text = element_text(color = "black"))

ggsave("Plots/Range and median in PLN.png", dpi  = 900, width = 20, height = 10, unit = "cm")


## Range for Brent PLN
# Calculating range for each year for brent in PLN
data %>%
  group_by(year) %>%
  filter(!year == 2004) %>%
  mutate(max = max(brentPLN), 
         min = min(brentPLN), 
         range = max - min, 
         range_mean = range / mean(brentPLN)) %>%
  select(year, range, range_mean) %>%
  filter(row_number() == 1) %>%
  ggplot(aes(x = year, y = range)) +
  geom_line(color = PLN_color, size = 0.75) +
  geom_point(shape = 21, size = 1.5, stroke = 1, fill = "white", color = PLN_color) +
  labs(x = NULL, y = NULL,
       caption = "Source: EIA, stooq") +
  ggtitle("Candlestick chart for Brent crude oil prices in PLN 2005-2022") +
  scale_y_continuous(breaks = seq(0,300, 50), 
                     limits = c(0,260), 
                     expand = c(0,0)) +
  scale_x_continuous(minor_breaks = seq(min(data$year), 2025, 1), 
                     guide = "axis_minor", 
                     breaks = seq(min(data$year) + 1, 2020, 5), 
                     expand = c(0,0), 
                     limits = c(min(data$year), max(data$year) + 1)) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 4),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        text = element_text(color = "black"))

ggsave("Plots/Range in PLN.png", dpi = 900, width = 20, height = 10, unit = "cm")


## Candlestick for brent in PLN
data_open_close <- data %>% 
  filter(year != 2004) %>%
  group_by(year) %>% 
  filter(Date == min(Date) | Date == max(Date)) %>% 
  summarise(year, open = last(brentPLN), close = first(brentPLN)) %>% 
  filter(row_number() == 1) %>% 
  mutate(color = ifelse(open - close > 0, "green", "red"))

data %>%
  filter(year != 2004) %>%
  group_by(year) %>% 
  inner_join(data_open_close %>% select(year, color), by = "year") %>% 
  mutate(median = median(brentPLN)) %>%
  ggplot(aes(x = year, y = brentPLN, group = year, color = color)) +
  geom_line() +
  geom_segment(data = data_open_close, aes(x = year, xend = year, y = open, yend = close, color = color), size = 2) +
  scale_color_manual(values = c("#DF484C", "#449682")) +
  labs(x = NULL, y = NULL,
       caption = "Source: EIA, stooq") +
  ggtitle("Candlestick chart for Brent crude oil prices in PLN 2005-2022") +
  scale_y_continuous(breaks = seq(0,600, 150), 
                     limits = c(0,650), 
                     expand = c(0,0)) +
  scale_x_continuous(minor_breaks = seq(2004, 2025, 1), 
                     guide = "axis_minor", 
                     limits = c(2004,(year(max(data$Date) + years(1)))), 
                     expand = c(0.,0)) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_markdown(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 4),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        legend.position = "none")

ggsave("Plots/Candlestick PLN.png", dpi  = 900, width = 20, height = 10, unit = "cm")


## Brent in USD

# Line plot
p1 <- data %>%
  filter(!year == 2004) %>%
  ggplot(aes(x = Date, y = brent)) + 
  geom_line() +
  labs(x = NULL, y = NULL) +
  ggtitle("Brent Crude Oil Market Price in USD 2005-2022") +
  scale_y_continuous(breaks = seq(0,150, 25), 
                     limits = c(0,165), 
                     expand = c(0,0)) +
  scale_x_date(date_labels = "%Y", 
               minor_breaks = seq.Date(as.Date("2005-01-01"), as.Date(now() + years(3)), by = "year"), 
               guide = "axis_minor") +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        text = element_text(color = "black"))

# Boxplots
p2 <- data %>%
  filter(!year == 2004) %>%
  ggplot(aes(x = year, y = brent, group = year)) + 
  geom_boxplot() +
  labs(x = NULL, y = NULL,
       caption = "Source: EIA, stooq") +
  ggtitle("Brent Crude Oil Market Price in USD 2005-2022") +
  scale_y_continuous(breaks = seq(0,150, 25), 
                     limits = c(0,165), 
                     expand = c(0,0)) +
  scale_x_continuous(minor_breaks = seq(2005, 2025, 1), 
                     guide = "axis_minor") +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(),
        plot.caption = element_text(hjust = 0, size = 4),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        text = element_text(color = "black"))

p_full <- p1 / p2

ggsave("Plots/Brent USD.png", p_full, dpi  = 900, width = 15, height = 20, unit = "cm")


## Range and median value for brent USD

data %>%
  filter(year != 2004) %>%
  group_by(year) %>% 
  mutate(median = median(brent)) %>%
  ggplot(aes(x = year, y = brent, group = year)) +
  geom_line() +
  geom_point(mapping = aes(y = median), shape = 22, color = USD_color, fill = "white", size = 2, stroke = 1) +
  labs(x = NULL, y = NULL,
       caption = "Source: EIA, stooq") +
  ggtitle("Range and <span style='color:#9EBE91;'>median</span> of Brent crude oil in USD, 2005-2022") +
  scale_y_continuous(breaks = seq(0,150, 25), 
                     limits = c(0,149), 
                     expand = c(0,0)) +
  scale_x_continuous(minor_breaks = seq(2004, 2025, 1), 
                     guide = "axis_minor", 
                     limits = c(2004,(year(max(data$Date) + years(1)))),
                     expand = c(0.,0)) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 4),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        text = element_text(color = "black"))

ggsave("Plots/Range and median in USD.png", dpi  = 900, width = 20, height = 10, unit = "cm")


## Range in USD
# Calculating range for each year for brent USD
data %>%
  group_by(year) %>%
  filter(!year == 2004) %>%
  mutate(max = max(brent), min = min(brent), range = max - min, range_mean = range / mean(brent)) %>%
  select(year, range, range_mean) %>%
  filter(row_number() == 1) %>%
  ggplot(aes(x = year, y = range)) +
  geom_line(color = USD_color) +
  geom_point(shape = 21, size = 2, stroke = 1, fill = "white", color = USD_color) +
  labs(x = NULL, y = NULL,
       caption = "Source: EIA, stooq") +
  ggtitle("Brent Crude Oil Market Price Range in USD 2005-2022") +
  scale_y_continuous(breaks = seq(0,110, 20), 
                     limits = c(0,110), 
                     expand = c(0,0)) +
  scale_x_continuous(minor_breaks = seq(min(data$year), 2025, 1), 
                     guide = "axis_minor", 
                     breaks = seq(min(data$year) + 1, 2020, 5), 
                     expand = c(0,0), 
                     limits = c(min(data$year), max(data$year) + 1)) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 4),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        text = element_text(color = "black"))

ggsave("Plots/Range in USD.png", dpi  = 900, width = 20, height = 10, unit = "cm")


## Candlestick for brent USD
data_open_close <- data %>% 
  filter(year != 2004) %>%
  group_by(year) %>% 
  filter(Date == min(Date) | Date == max(Date)) %>% 
  summarise(year, open = last(brent), close = first(brent)) %>% 
  filter(row_number() == 1) %>% 
  mutate(color = ifelse(open - close > 0, "green", "red"))

# Plot
data %>%
  filter(year != 2004) %>%
  group_by(year) %>% 
  inner_join(data_open_close %>% select(year, color), by = "year") %>% 
  mutate(median = median(brent)) %>%
  ggplot(aes(x = year, y = brent, group = year, color = color)) +
  geom_line() +
  geom_segment(data = data_open_close, aes(x = year, xend = year, y = open, yend = close, color = color), size = 2) +
  scale_color_manual(values = c("#DF484C", "#449682")) +
  labs(x = NULL, y = NULL,
       caption = "Source: EIA, stooq") +
  ggtitle("Candlestick chart for Brent crude oil prices in USD 2005-2022") +
  scale_y_continuous(breaks = seq(0,150, 25), 
                     limits = c(0,160), 
                     expand = c(0,0)) +
  scale_x_continuous(minor_breaks = seq(2004, 2025, 1), 
                     guide = "axis_minor", 
                     limits = c(2004,(year(max(data$Date) + years(1)))), 
                     expand = c(0.,0)) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_markdown(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 4),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        legend.position = "none")

ggsave("Plots/Candlestick USD.png", dpi  = 900, width = 20, height = 10, unit = "cm")


## Brent in USD vs brent in PLN

# Base plot
p <- data %>% 
  filter(!year == 2004) %>% # Filter for 2004 as for this year data is not full
  mutate(brent_rel = 100 * brent / last(brent),
         brentPLN_rel = 100 * brentPLN / last(brentPLN)) %>%
  ggplot(aes(x = Date)) + 
  geom_line(aes(y = brent_rel, color = "Brent USD")) +
  geom_line(aes(y = brentPLN_rel, color = "Brent PLN")) +
  labs(x = NULL, y = NULL, 
       subtitle = "100 = price level on January 1, 2005",
       caption = "Source: EIA, stooq") +
  ggtitle("Relative growth of Brent crude oil in USD and PLN since 2005")

# Making a plot more pretty
p <- p +
  scale_y_continuous(breaks = seq(0,400, 100), 
                     limits = c(0,450), 
                     expand = c(0,0)) +
  scale_x_date(limits = c(floor_date(min(data$Date) - years(1), "year"), floor_date(max(data$Date) + years(3), "year")), 
               expand = c(0,0),
               breaks = seq.Date(as.Date("2005-01-01"), as.Date("2020-01-01"), "5 years"),
               date_labels = "%Y", 
               minor_breaks = seq.Date(as.Date("2003-01-01"), as.Date(now() + years(4)), by = "year"), 
               guide = "axis_minor") +
  scale_color_manual(name = NULL, 
                     values = c(PLN_color, USD_color)) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(hjust = 0, size = 4),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "none")

# Preparing shortcuts
brent_fl <- 100 * first(data$brent) / last(data$brent)
brentPLN_fl <- 100 * first(data$brentPLN) / last(data$brentPLN)

# Adding text, curves and legend to the plot
p <- p +
  geom_curve(data = data.frame(x = as.Date("2017-10-19"),
                               y = 359.118961791378, xend = as.Date("2021-12-05"),
                               yend = 376.650791546578),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
             curvature = -0.105, arrow = arrow(30L, unit(0.05, "inches"),
                                               "last", "closed"),
             size = 0.4, inherit.aes = FALSE) + 
  geom_curve(data = data.frame(x = as.Date("2006-04-14"),
                               y = 330.055302280977, xend = as.Date("2008-01-07"),
                               yend = 268.830038552096),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
             curvature = 0.4, arrow = arrow(30L, unit(0.05, "inches"),
                                            "last", "closed"),
             size = 0.4, inherit.aes = FALSE) + 
  geom_curve(data = data.frame(x = as.Date("2007-11-07"),
                               y = 85.6224176102539, xend = as.Date("2008-01-26"),
                               yend = 156.626328118815),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
             curvature = 0.2, arrow = arrow(30L, unit(0.05, "inches"),
                                            "last", "closed"),
             size = 0.4, inherit.aes = FALSE) + 
  geom_text(
    data = data.frame(
      x = as.Date("2006-04-14"),
      y = 358.982821377058,
      label = "From early 2006 to mid-2008\nthe price of a barrel of oil in USD\nincreased 2.5 times. The decline occurred\nat the beginning of the 2008 financial crisis."
    ),
  mapping = aes(x = x, y = y, label = label),
  size = 2, inherit.aes = FALSE
  ) +
  geom_text(data = data.frame(x = as.Date(c("2017-10-19", "2007-11-07" )),
                              y = c(331.821217158577, 58.0651240508935),
                              label = c("The war in Ukraine, high inflation, the first \n signs of economic slowdown \n led to the depreciation of the PLN against the USD \n and an even stronger increase in the price of a barrel of oil", "The złoty in the times before the financial crisis \n appreciated against the US dollar \n which is why the global increase in the price of a barrel of oil was not \n felt so strongly in Poland" )),
            mapping = aes(x = x, y = y, label = label),
            size = 2, inherit.aes = FALSE) +
  geom_curve(data = data.frame(x = as.Date(c(max(data$Date), max(data$Date))),
                               y = c(brent_fl, brentPLN_fl), xend = as.Date(c(max(data$Date) + days(550), max(data$Date) + days(550))),
                               yend = c(brent_fl, brentPLN_fl)),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
             curvature = 0,
             color = c(USD_color, PLN_color), size = 0.2, inherit.aes = FALSE) +
  geom_text(data = data.frame(x = as.Date(c(max(data$Date) + days(150), max(data$Date) + days(150))),
                              y = c(brent_fl, brentPLN_fl),
                              label = c(glue("Brent USD\n\n{direction} {round(brent_fl)}%\n42 → {round(first(data$brent))} USD", direction = ifelse(brent_fl >= 0, " increase by", "decrease by")), glue("Brent PLN\n\n{direction} {round(brentPLN_fl)}%\n130 → {round(first(data$brentPLN))} PLN", direction = ifelse (brentPLN_fl >= 0, "increase by", "decrease by")))),
            mapping = aes(x = x, y = y, label = label),
            color = c(USD_color, PLN_color), size = 2, inherit.aes = FALSE, hjust = 0, vjust = 0.77)

# Adding points on the end of the line plots
p <- p +
  geom_point(aes(x = max(Date), y = brent_fl, color = "Brent USD"), shape = 21, size = 1, fill = "white") +
  geom_point(aes(x = max(Date), y = brentPLN_fl, color = "Brent PLN"), shape = 21, size = 1, fill = "white")

ggsave("Plots/Brent in USD vs brent in PLN.png", p, dpi  = 900, width = 20, height = 15, unit = "cm")


# Read fuel price data
fuel_price_EU <- read_csv("Processed_data/fuel_price_EU.csv") %>% 
  mutate(date = as.Date(date))

## Pb95 price among EU members
# Data to plot prices on chart
fuel_price_EU_growth_y_to_y <- fuel_price_EU %>%
  group_by(code) %>%
  filter(row_number() == 8 | date == max(date) - weeks(60)) %>%
  mutate(
    growth = 100 * Euro_super_95[1] / Euro_super_95[2] - 100, 
    growth = ifelse(growth == 0, 0.0001, growth)) %>%
  filter(row_number() == 1) 

# Create a color palette
palette <- wesanderson::wes_palette("Darjeeling1", n = 5, type = "continuous")

# Make a tibble to use in correct order as colors on the plot 
countries_letters <- tibble(code = c("AT", "DE", "MT", "HU", "PL"), 
                            color = c("B", "D", "C", "E", "A"), 
                            names = c("Austria", "Germany", "Malta", "Hungary", "Poland")) %>% 
  arrange(code)

# Join two data frames and keep only selected countries
countries <- tibble(fuel_price_EU) %>% right_join(countries_letters, by = "code") 

# Pull gasoil prices
y_line <- countries %>% 
  group_by(code) %>% 
  filter(row_number() == 1) %>% 
  pull(Euro_super_95)

# Plot 
fuel_price_EU %>%
  ggplot(aes(x = date, y = Euro_super_95, group = code)) +
  # Make a line that goes from the end point on the right to a country name
  geom_curve(data = data.frame(x = as.Date(max(fuel_price_EU$date)),
                               y = y_line, xend = as.Date(max(fuel_price_EU$date) + days(170)),
                               yend = y_line),
             mapping = aes(x = x, y = y, xend = xend, yend = yend, color = unique(countries$color)),
             curvature = 0, size = 0.2, inherit.aes = FALSE) +
  # Make a country name label
  geom_text(data = data.frame(x = as.Date(max(fuel_price_EU$date) + days(180)),
                              y = y_line),
            mapping = aes(x = x, y = y, color = unique(countries$color), label = countries_letters$names),
            size = 3, inherit.aes = FALSE, hjust = 0, fontface = "bold") +
  # Add lines for other countries
  geom_line(aes(color = "F"),
            size = 0.3) +
  # Add lines for selected countries 
  geom_line(data = countries, aes(color = color), 
            size = 0.75) +
  # Add points for each of the selected counties on the right side
  geom_point(data = countries %>% group_by(code) %>% filter(row_number() == 1), 
             aes(color = color), 
             size = 1, shape = 21, fill = "white", stroke = 1) +
  scale_color_manual(values = c(palette, "#F0F0F0"), 
                     name = NULL) +
  scale_x_date(limits = c(floor_date(as.Date(min(fuel_price_EU$date)), unit = "year"), floor_date(as.Date(max(fuel_price_EU$date)) + years(2), unit = "year")), 
               expand = c(0,0),
               breaks = seq.Date(as.Date("2012-01-01"), as.Date("2023-01-01"), "2 years"),
               date_labels = "%Y", 
               minor_breaks = seq.Date(as.Date("2012-01-01"), as.Date(now()) + years(2), by = "year"), 
               guide = "axis_minor") +
  scale_y_continuous(breaks = seq(0, 2.5, 0.5), 
                     labels = paste0(seq(0, 2.5, 0.5), "€")) +
  labs(x = NULL, y = NULL, 
       title = paste("Pb95 price among EU Member States on", format(max(fuel_price_EU$date), "%d %B %Y")), 
       caption = "Source: Oil Bulletin. European Commission, 2022") + 
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(hjust = 0, size = 4),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "none")

ggsave("Plots/Pb95 price among EU members.png", dpi = 900, width = 22, height = 15, unit = "cm")


 ## Increase in Pb95 fuel prices among EU members
# Data for year to year increase in price
data_y_to_y <- fuel_price_EU %>%
  group_by(code) %>%
  filter(row_number() < 52) %>%
  # filter(date > as.Date("2022-01-01")) %>%
  mutate(code = factor(code), 
         growth = 100 *(Euro_super_95 / last(Euro_super_95) - 1), 
         growth = ifelse(growth == 0, 0.0001, growth), 
         is_poland = code == "PL")

# Find top 3 countries that have had the biggest growth in Pb95 price
data_y_to_y %>% 
  filter(row_number() == 1) %>% 
  summarise(max = max(growth)) %>% 
  arrange(desc(max)) %>% 
  head(n = 3)

# Filter for interesting countries to hightlight them in a plot
countries <- data_y_to_y %>%
  filter(code %in% c("AT", "DE", "MT", "HU", "PL")) %>%
  group_by(code) %>%
  arrange(code)

# Create a tibble with names and letters. If you want to highlight different countries change code and name
# Colors are used to better differentiate the countries between each other. Otherwise some of the countries would have
# too similar colors and it would be hard to distinguish them. 
countries_letters <- tibble(code = c("AT", "DE", "MT", "HU", "PL"), 
                            color = c("B", "D", "C", "E", "A"), 
                            names = c("Austria", "Germany", "Malta", "Hungary", "Poland")) %>%
  arrange(code)

# Add countries letters to countries data
countries %<>% inner_join(countries_letters, by = "code")

# Make a line for selected countries
y_line <- countries %>% group_by(code) %>% filter(row_number() == 1) %>% pull(growth)

# Create a color palette
palette <- wesanderson::wes_palette("Darjeeling1", n = 5, type = "continuous")

# Plot chart
ggplot(data_y_to_y, aes(x = date, y = growth, group = code)) +
  geom_curve(data = data.frame(x = as.Date(max(data_y_to_y$date)),
                               y = y_line, xend = as.Date(max(data_y_to_y$date)) + days(13),
                               yend = y_line),
             mapping = aes(x = x, y = y, xend = xend, yend = yend, color = unique(countries$color)),
             curvature = 0, size = 0.2, inherit.aes = FALSE) +
  geom_text(data = data.frame(x = as.Date(max(data_y_to_y$date)) + days(16),
                              y = y_line),
            mapping = aes(x = x, y = y, color = unique(countries$color), label = countries_letters$names),
            size = 3, inherit.aes = FALSE, hjust = 0, fontface = "bold") +
  geom_line(color = "#F0F0F0",
            size = 0.5) +
  geom_line(data = countries, aes(color = color), 
            size = 0.75) +
  geom_point(data = countries %>% group_by(code) %>% filter(row_number() == 1), aes(color = color), 
             size = 1, shape = 21, fill = "white", stroke = 1) +
  scale_color_manual(values = c(palette, "#F0F0F0"), 
                     name = NULL) +
  scale_x_date(limits = c(floor_date(as.Date(min(data_y_to_y$date)), unit = "month"), ceiling_date(as.Date(max(data_y_to_y$date)), unit = "month") + months(1)), 
               expand = c(0,0),
               breaks = seq.Date(floor_date(as.Date(min(data_y_to_y$date)), unit = "month"), ceiling_date(as.Date(max(data_y_to_y$date)), unit = "month"), "3 months"),
               date_labels = "%b %y", 
               minor_breaks = seq.Date(floor_date(as.Date(min(data_y_to_y$date)), unit = "month"), ceiling_date(as.Date(max(data_y_to_y$date)) + months(1), unit = "month") + days(1), by = "1 month"), 
               guide = "axis_minor") +
  labs(x = NULL, y = NULL,
       title = paste("Growth of Pb95 price among EU Member States from", format(as.Date(min(data_y_to_y$date)), "%d %B %Y"), "until", format(as.Date(max(data_y_to_y$date)), "%d %B %Y")),
       caption = "Source: Oil Bulletin. European Commission, 2022") + 
  scale_y_continuous(labels = paste0(seq(0,60,20), "%"), 
                     breaks = seq(0,60,20)) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(hjust = 0, size = 4),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "none")

ggsave("Plots/Increase in Pb95 fuel prices among EU members.png", dpi = 900, width = 22, height = 15, units = "cm")
