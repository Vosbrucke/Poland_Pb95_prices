library(wesanderson)
library(ggtext)
library(tidyverse)
library(magrittr)
library(lubridate)
library(patchwork)
library(ggh4x)

# Wczytaj dane
data <-read_csv('/Processed_data/full_data.csv')

# Modelowa cena w stosunku do ceny realnej. Różnice wynikają z założeń modelu w postaci stałej wielkości marży stacji oraz realiów rynkowych- dostosowanie do wahań nie następuje natychmiastowo. 
# Cena modelowa została obliczona poprzez zsumowanie wszystkich czynników cenotwórczych dla paliwa Pb95.
palette <- wesanderson::wes_palette("Zissou1", 7, type = "continuous")

data %>% filter(Date > as.Date("2019-11-30")) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = price_model, color = "1"), size = 0.6, linetype = 5) +
  geom_line(aes(y = price, color = "2"), size = 0.6) +
  geom_line(aes(y = price_hurt, color = "3")) +
  geom_line(aes(y = excise, color = "4")) +
  geom_line(aes(y = VAT, color = "5")) +
  geom_line(aes(y = store_margin, color = "6")) +
  geom_line(aes(y = fuel_surcharge, color = "7")) +
  geom_line(aes(y = emission_tax, color = "8")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,9), breaks = seq(0,8,2)) +
  scale_x_date(limits = c(as.Date("2019-11-30"), NA), expand = c(0,0)) +
  scale_color_manual(name = NULL, values = c("#666666", palette), labels = c("Cena modelowa", "Cena detaliczna", "Cena hurtowa", "Podatek emisyjny", "VAT", "Opłata paliwowa", "Akcyza", "Marża stacji")) +
  theme_classic() +
  labs(x = NULL, y = NULL, title = "Ceny poszczególnych czynników wchodzących w skład ceny detalicznej paliwa Pb95", subtitle = "Cena modelowa stanowi sumę cen czynników cenotwórczych dla paliwa Pb95") +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(face = "bold"),
        plot.subtitle = element_text(size = 8),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        text = element_text(color = "black"))

ggsave("Plots/Model_vs_actual_price.png", dpi = 900, width = 20, height = 10, unit = "cm")

# Cena hurtowa vs cena brent wyrażona w PLN
palette <- wesanderson::wes_palette("Zissou1", 2, type = "continuous")

data %>% filter(Date > as.Date("2022-01-01")) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = 100 * price_hurt / last(price_hurt), color = "Cena hurtowa"), size = 0.75) +
  geom_line(aes(y = 100 * brentPLN / last(brentPLN), color = "Brent Oil PLN"), size = 0.75) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  scale_color_manual(name = NULL, values = c("Cena hurtowa" = palette[1], "Brent Oil PLN" = "#E4B80E")) +
  labs(x = NULL, y = NULL, title = 'Cena w hurcie "oderwała" się od notowań Brent Oil', subtitle = "100 = poziom cen w dniu 01.01.2022") +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(face = "bold"),
        plot.subtitle = element_text(size = 8),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        text = element_text(color = "black"),
        axis.text.x = element_text(angle = 0))

ggsave("Plots/Wholesale_vs_brent_price.png", dpi = 900, width = 20, height = 10, unit = "cm")

# Wizualizacja czynników cenotwórczych dla paliwa Pb95

# Przygotowanie danych
share_data <- data %>% select(Date, price_hurt, excise, VAT, store_margin, emission_tax, fuel_surcharge)

share_data  <- share_data %>% pivot_longer(cols = -Date, values_to = "price") %>% mutate(name = (factor(name, levels = c("price_hurt", "excise", "VAT", "store_margin", "fuel_surcharge", "emission_tax"))))

palette <- wesanderson::wes_palette("Zissou1", 6, type = "continuous")

share_data %<>% filter(Date > as.Date("2020-01-01"))

# Oczyszczenie danych poprzez zastąpienie daty dla obserwacji najstarszych na właściwą dla celów wizualizacyjnych
index <- which(share_data$Date == min(share_data$Date))
share_data$Date[index] <- floor_date(share_data$Date[index], unit = "month")


share_data %>%
  ggplot(aes(fill = name, x = Date, y = price)) + 
  geom_bar(position = "fill", stat = "identity", width = 15) +
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
  scale_x_date(date_breaks = "6 months", date_labels = "%b %y", limits = c(floor_date(min(share_data$Date), "month"), max(share_data$Date))) + 
  coord_cartesian(expand = c(0,0)) +
  labs(y = NULL, x = NULL, title = "Procentowy udział poszczególnych czynników cenotwórczych paliwa Pb95 w Polsce od 2020 r.") +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(face = "bold", size = 9, hjust = 0.1),
        axis.line = element_blank(),
        axis.text.x = element_text(color = "black", angle = 90),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(color = "black"))

ggsave("Plots/Czynniki_cenotwórcze_Pb95.png", dpi  = 900, width = 20, height = 10, unit = "cm")

# Wybór kolorów dla ceny ropy brent w USD i PLN
# Choosing colors for brent oil in USD and PLN
palette <- wesanderson::wes_palette("Zissou1", 10, type = "continuous")

# 4 i 7 kolor z palety zostały wybrane na podstawie identyfikacji wizualnej (zielony dla USD, złoty dla PLN)
# 4th and 7th color chosen by associated with USD and PLN visual identification (green for USD, gold for PLN)
USD_color <- "#9EBE91"
PLN_color <- "#E4B80E"

# Only brent PLN data
# Line plot
p1 <- data %>%
  filter(!year == 2004) %>%
  ggplot(aes(x = Date, y = brentPLN)) + 
  geom_line() +
  labs(x = NULL, y = NULL) +
  ggtitle("Ropa brent w PLN w latach 2005-2022") +
  scale_y_continuous(breaks = seq(0,600, 150), limits = c(0,650), expand = c(0,0)) +
  scale_x_date(date_labels = "%Y", minor_breaks = seq.Date(as.Date("2005-01-01"), as.Date(now() + years(3)), by = "year"), guide = "axis_minor") +
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
  labs(x = NULL, y = NULL) +
  ggtitle("Ropa brent w PLN w latach 2005-2022") +
  scale_y_continuous(breaks = seq(0,600, 150), limits = c(0,650), expand = c(0,0)) +
  scale_x_continuous(minor_breaks = seq(2005, 2025, 1), guide = "axis_minor") +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        text = element_text(color = "black"))

p_full <- p1 / p2

ggsave("Plots/Boxplot_PLN.png", dpi  = 900, width = 15, height = 20, unit = "cm")


# Rozstęp i mediana wartości
data %>%
  filter(year != 2004) %>%
  group_by(year) %>% 
  mutate(median = median(brentPLN)) %>%
  ggplot(aes(x = year, y = brentPLN, group = year)) +
  geom_line() +
  geom_point(mapping = aes(y = median), shape = 22, color = PLN_color, fill = "white", size = 2, stroke = 1) +
  labs(x = NULL, y = NULL) +
  ggtitle("Rozstęp i <span style='color:#E4B80E;'>mediana</span> ropy brent w PLN w latach 2005-2022") +
  scale_y_continuous(breaks = seq(0,600, 150), limits = c(0,650), expand = c(0,0)) +
  scale_x_continuous(minor_breaks = seq(2005, 2025, 1), guide = "axis_minor") +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(face = "bold"),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        text = element_text(color = "black"))

ggsave("Plots/Rozstęp_i_mediana_w_PLN.png", dpi  = 900, width = 20, height = 10, unit = "cm")
  
# Obliczanie rozstępu dla każdego roku
data %>%
  group_by(year) %>%
  filter(!year == 2004) %>%
  mutate(max = max(brentPLN), min = min(brentPLN), range = max - min, range_mean = range / mean(brentPLN)) %>%
  select(year, range, range_mean) %>%
  filter(row_number() == 1) %>%
  ggplot(aes(x = year, y = range)) +
  geom_line(color = PLN_color) +
  geom_point(shape = 21, size = 2, stroke = 1, fill = "white", color = PLN_color) +
  labs(x = NULL, y = NULL) +
  ggtitle("Zakres notowań ropy brent w PLN w latach 2005-2022") +
  scale_y_continuous(breaks = seq(0,300, 50), limits = c(0,260), expand = c(0,0)) +
  scale_x_continuous(minor_breaks = seq(2005, 2025, 1), guide = "axis_minor") +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(face = "bold"),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        text = element_text(color = "black"))

ggsave("Plots/Rozstęp_w_PLN.png", dpi = 900, width = 20, height = 10, unit = "cm")

plotly::ggplotly(p1)
## Only brent USD data
# Line plot
p1 <- data %>%
  filter(!year == 2004) %>%
  ggplot(aes(x = Date, y = brent)) + 
  geom_line() +
  labs(x = NULL, y = NULL) +
  ggtitle("Ropa brent w USD w latach 2005-2022") +
  scale_y_continuous(breaks = seq(0,150, 25), limits = c(0,165), expand = c(0,0)) +
  scale_x_date(date_labels = "%Y", minor_breaks = seq.Date(as.Date("2005-01-01"), as.Date(now() + years(3)), by = "year"), guide = "axis_minor") +
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
  labs(x = NULL, y = NULL) +
  ggtitle("Ropa brent USD w latach 2005-2022") +
  scale_y_continuous(breaks = seq(0,150, 25), limits = c(0,165), expand = c(0,0)) +
  scale_x_continuous(minor_breaks = seq(2005, 2025, 1), guide = "axis_minor") +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        text = element_text(color = "black"))

p_full <- p1 / p2

ggsave("Plots/Boxplot_USD.png", dpi  = 900, width = 15, height = 20, unit = "cm")

# Rozstęp i mediana wartości
data %>%
  filter(year != 2004) %>%
  group_by(year) %>% 
  mutate(median = median(brent)) %>%
  ggplot(aes(x = year, y = brent, group = year)) +
  geom_line() +
  geom_point(mapping = aes(y = median), shape = 22, color = USD_color, fill = "white", size = 2, stroke = 1) +
  labs(x = NULL, y = NULL) +
  ggtitle("Rozstęp i <span style='color:#9EBE91;'>mediana</span> ropy brent w USD w latach 2005-2022") +
  scale_y_continuous(breaks = seq(0,150, 25), limits = c(0,149), expand = c(0,0)) +
  scale_x_continuous(minor_breaks = seq(2005, 2025, 1), guide = "axis_minor") +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(face = "bold"),
        panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        text = element_text(color = "black"))

ggsave("Plots/Rozstęp_i_mediana_w_USD.png", dpi  = 900, width = 20, height = 10, unit = "cm")

# Obliczanie rozstępu dla każdego roku
data %>%
  group_by(year) %>%
  filter(!year == 2004) %>%
  mutate(max = max(brent), min = min(brent), range = max - min, range_mean = range / mean(brent)) %>%
  select(year, range, range_mean) %>%
  filter(row_number() == 1) %>%
    ggplot(aes(x = year, y = range)) +
    geom_line(color = USD_color) +
    geom_point(shape = 21, size = 2, stroke = 1, fill = "white", color = USD_color) +
    labs(x = NULL, y = NULL) +
    ggtitle("Zakres notowań ropy brent w USD w latach 2005-2022") +
    scale_y_continuous(breaks = seq(0,110, 20), limits = c(0,110), expand = c(0,0)) +
    scale_x_continuous(minor_breaks = seq(2005, 2025, 1), guide = "axis_minor") +
    theme_classic() +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(face = "bold"),
          panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          text = element_text(color = "black"))

ggsave("Plots/Rozstęp_w_USD.png", dpi  = 900, width = 20, height = 10, unit = "cm")

## USD and PLN data together
# Base plot
p <- data %>% 
  filter(!year == 2004) %>% # Filter for 2004 as for this year data is not full
  mutate(brent_rel = 100 * brent / last(brent),
         brentPLN_rel = 100 * brentPLN / last(brentPLN)) %>%
  ggplot(aes(x = Date)) + 
  geom_line(aes(y = brent_rel, color = "Brent USD")) +
  geom_line(aes(y = brentPLN_rel, color = "Brent PLN")) +
  labs(x = NULL, y = NULL, subtitle = "100 = poziom ceny 1 stycznia 2005 roku") +
  ggtitle("Relatywny wzrost ropy brent w USD i PLN od 2005 roku")

# Making a plot more pretty
p <- p +
  scale_y_continuous(breaks = seq(0,400, 100), limits = c(0,450), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date(min(data$Date) - days(350)), as.Date(max(data$Date) + days(800))), expand = c(0,0),
               breaks = seq.Date(as.Date("2005-01-01"), as.Date("2020-01-01"), "5 years"),
               date_labels = "%Y", minor_breaks = seq.Date(as.Date("2005-01-01"), as.Date(now() + years(4)), by = "year"), guide = "axis_minor") +
  scale_color_manual(name = NULL, values = c(PLN_color, USD_color)) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 8),
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
  geom_text(data = data.frame(x = as.Date("2006-04-14"),
                              y = 358.982821377058, label = "Od początku 2006 r. do połowy 2008 r. \n cena za baryłkę ropy naftowej w USD \n wzrosła 2.5-krotnie. Spadek nastąpił \n na początku kryzysu finansowego z 2008 r."),
            mapping = aes(x = x, y = y, label = label),
            size = 2, inherit.aes = FALSE) + 
  geom_text(data = data.frame(x = as.Date(c("2017-10-19", "2007-11-07" )),
                              y = c(331.821217158577, 58.0651240508935),
                              label = c("Wojna na Ukranie, wysoka inflacja, pierwsze \n oznaki spowolnienia gospodarczego \n doprowadziły do deprecjacji PLN w stosunku do USD \n i tym silniejszego wzrostu ceny za baryłkę ropy", "Złotówka w czasach sprzed kryzysu finansowego \n aprecjonowała względem dolara amerykańskiego \n przez co światowy wzrost baryłki ropy nie był aż \n tak silnie w Polsce odczuwany" )),
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
                              label = c(glue("Brent USD\n\n{direction} {round(brent_fl)}%\n42 → {round(first(data$brent))} USD", direction = ifelse(brent_fl >= 0, "wzrost o", "spadek o")), glue("Brent PLN\n\n{direction} {round(brentPLN_fl)}%\n130 → {round(first(data$brentPLN))} PLN", direction = ifelse(brentPLN_fl >= 0, "wzrost o", "spadek o")))),
            mapping = aes(x = x, y = y, label = label),
            color = c(USD_color, PLN_color), size = 2, inherit.aes = FALSE, hjust = 0, vjust = 0.77)

# Adding points on the end of the line plots
p <- p +
  geom_point(aes(x = max(Date), y = brent_fl, color = "Brent USD"), shape = 21, size = 1, fill = "white") +
  geom_point(aes(x = max(Date), y = brentPLN_fl, color = "Brent PLN"), shape = 21, size = 1, fill = "white")

ggsave("Plots/Brent_USD_PLN.png", p, dpi  = 900, width = 20, height = 15, unit = "cm")
