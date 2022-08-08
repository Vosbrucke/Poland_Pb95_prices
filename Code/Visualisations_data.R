library(wesanderson)
library(ggtext)
library(tidyverse)
library(magrittr)
library(lubridate)

# Modelowa cena w stosunku do ceny realnej. Różnice wynikają z założeń modelu w postaci stałej wielkości marży stacji i zmienności rynku.
palette <- wesanderson::wes_palette("Zissou1", 6, type = "continuous")

data %>% filter(Date > as.Date("2020-01-01")) %>%
  ggplot() +
  geom_line(aes(x = Date, y = price, color = "Cena detaliczna"), color = "black") +
  geom_line(aes(x = Date, y = price_model, color = "Cena modelowa"), color = "darkgreen") +
  geom_line(aes(x = Date, y = price_hurt, color = "Cena hurtowa")) +
  geom_line(aes(x = Date, y = VAT, color = "VAT")) +
  geom_line(aes(x = Date, y = emission_tax, color = "Podatek emisyjny")) +
  geom_line(aes(x = Date, y = fuel_surcharge, color = "Opłata paliwowa")) +
  geom_line(aes(x = Date, y = excise, color = "Akcyza")) +
  geom_line(aes(x = Date, y = store_margin, color = "Marża stacji")) +
  geom_hline(yintercept = 0) +
  scale_color_manual(name = NULL, values = c(palette)) +
  theme_minimal() +
  labs(x = NULL, y = NULL, title = "Wartość poszczególnych czynników cenotwórczych dla paliwa Pb95") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("Plots/Model_vs_actual_price.png", dpi = 900, width = 10, height = 10, unit = "cm")

# Wholesale vs Brent price visualization
palette <- wesanderson::wes_palette("Zissou1", 2, type = "continuous")

# Making a plot for how wholesale and brent prices are connected
data %>% filter(Date > as.Date("2022-01-01")) %>%
  ggplot() +
  geom_line(aes(x = Date, y = 100 * price_hurt / last(price_hurt), color = "Cena hurtowa"), size = 0.75) +
  geom_line(aes(x = Date, y = 100 * brentPLN / last(brentPLN), color = "Brent Oil"), size = 0.75) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_color_manual(name = NULL, values = c("Cena hurtowa" = palette[1], "Brent Oil" = palette[2])) +
  # labs(x = NULL, y = NULL, title = "<span style='color:#3B9AB2;'>Cena w hurcie</span> oderwała się od notowań <span style='color:#F21A00;'>Brent Oil</span>") +
  labs(x = NULL, y = NULL, title = 'Cena w hurcie "oderwała" się od notowań Brent Oil', subtitle = "100 = poziom cen w dniu 01.01.2022") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_markdown(),
        plot.subtitle = element_text(size = 8),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90))

ggsave("Plots/Wholesale_vs_brent_price.png", dpi = 900, width = 10, height = 10, unit = "cm")


# Price factors visualization
share_data <- data %>% select(Date, price_hurt, excise, VAT, store_margin, emission_tax, fuel_surcharge)

share_data  <- share_data %>% pivot_longer(cols = -Date, values_to = "price") %>% mutate(name = (factor(name, levels = c("price_hurt", "excise", "VAT", "store_margin", "fuel_surcharge", "emission_tax"))))

palette <- wesanderson::wes_palette("Zissou1", 6, type = "continuous")

share_data %<>% filter(Date > as.Date("2020-01-01"))

share_data %>%
  ggplot(aes(fill=name, x=Date, y=price)) + 
  geom_bar(position="fill", stat="identity", width = 15) +
  geom_vline(xintercept = as.Date("2022-02-03"), color = "#333333") +
  geom_curve(data = data.frame(x = as.Date("2021-10-01"),
                               y = 0.747391474149104, xend = as.Date("2022-01-09"),
                               yend = 0.583470506718382),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
             curvature = 0.49, arrow = arrow(20L, unit(0.1, "inches"),
                                             "last", "closed"),
             inherit.aes = FALSE, color = "#333333") +
  geom_text(data = data.frame(x = as.Date("2021-09-28"),
                              y = 0.818102479707454, label = "Obniżka VAT na paliwo \n z 23% do 8%"),
            mapping = aes(x = x, y = y, label = label),
            inherit.aes = FALSE, color = "#333333") +
  scale_fill_manual(values = palette, labels = c("cena hurtowa", "akcyza", "VAT", "marża stacji", "opłata paliwowa", "podatek emisyjny"), name = NULL) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", limits = c(floor_date(min(share_data$Date), "month"), max(share_data$Date))) + 
  coord_cartesian(expand = c(0,0)) +
  labs(y = NULL, x = NULL, title = "Procentowy udział poszczególnych czynników cenotwórczych paliwa Pb95 w Polsce od 2020 r.") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0))

ggsave("Plots/Czynniki_cenotwórcze_Pb95.png", dpi  = 900, width = 20, height = 10, unit = "cm")
