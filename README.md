# Polish_Gas_station_prices
Why are gas station prices for 95 oil so high in Poland?


## Ładowanie pakietów

``` {r}
# librarydevtools::install_github('bbc/bbplot')
# path <- "https://github.com/bbc/bbplot"
# install.packages(path, repos = NULL, type = "source")

library(bbplot)
library(devtools)
library(tidyverse)
library(ggtext)
```


## Wczytywanie danych

```{r}
new_BRENT <- read.csv("/Users/mr.fox/Desktop/Brent Oil Futures Weekly.csv") %>% 
  mutate(Date = mdy(Date)) %>% 
  filter(Date <= "2022-06-15") %>%
  select(1,2,7)

cena_paliwa <- read.csv("/Users/mr.fox/Desktop/Cena 95 w Polsce.csv", sep = ";", dec = ",") 

new_ceny_paliwa <- read.csv("/Users/mr.fox/Desktop/Cena 95 w Polsce tygodniowo.csv", sep = ";", dec = ",") %>% 
  mutate(DDate = dmy(Ddate)) %>% 
  filter(DDate <= "2022-06-15", DDate >= "2010-07-02") %>%
  select(-Ddate, - DDate)

new_USD_PLN <- read.csv("/Users/mr.fox/Desktop/USD_PLN Historical Weekly.csv") %>% 
  mutate(Date = mdy(Date)) %>% 
  filter(Date <= "2022-06-15") %>%
  select(1,2,6)
```


## Analiza danych

``` {r}

ceny_hurt <- cena_paliwa %>%
  mutate(date = dmy(Date), year = year(date), week = week(date), month = month(date)) %>%
  group_by(year, week) %>%
  summarise(mean_hurt = mean(Price)) %>%
  ungroup() %>%
  #mutate(year = as.numeric(year), week = as.numeric(week)) 
  arrange(desc(year), desc(week)) %>%
  mutate(year = as.numeric(year), week = as.numeric(week)) %>%
  mutate(Date = as.Date(paste(year, week, 7, sep="-"), "%Y-%U-%u")) %>%
  drop_na() %>%
  slice_head(n = 624) %>%
  slice_tail(n = 622) %>%
  select(-c(year, week))
  
new_df <- new_BRENT %>%
  left_join(new_USD_PLN, by = "Date") %>%
  cbind(new_ceny_paliwa) %>%
  left_join(ceny_hurt, by = "Date") %>%
  mutate(mean_hurt = ifelse(is.na(mean_hurt), mean_hurt[+1], mean_hurt))


new_df_1 <- new_df %>%
  #arrange(Date) %>%
  mutate(brent_pln = Price.x * Price.y,
         brent_change = (brent_pln[] / brent_pln[-1] - 1)*100,
         brent_change = ifelse(abs(brent_change) > 40, NA, brent_change),
         real_raf = mean_hurt / Gasoline,
         other_factors = Gasoline - mean_hurt,
         model_Gasoline = (1 + brent_change/100) * real_raf * Gasoline[-1] + other_factors
         )
```


## Wizualizacja danych. Przyznam szczerze, że te obliczenia nie podobają mi się do końca. Trzeba tam kilka rzeczy pozmieniać. Teraz jak patrzę to też nie pamiętam czemu niektóre z kroków robiłem. Dobrze byłoby prześledzić jak się zmieniała cena hurtowa vs cena detaliczna. Czy procentowo nie zwiększał się udział kosztów niezwiązanych z samą ceną baryłki tzn. opłata akcyzowa, paliwowa, emisyjna itp. Tutaj jest rozdzielenie ceny paliwa wg danych kategorii składników ceny: https://www.e-petrol.pl/notowania/pomocne-informacje/podzial-skladnikow-cen-paliw

### Poza tym należałoby stwierdzić co odpowiada za wzrost ceny hurtowej za paliwo. 

### PS. Ja póki co patrzyłem na ceny Pb95. Nie sprawdzałem ON, ponieważ uznałem, że powinien się ruszać podobnie.

``` {r}
##### PROTIP: Usuwanie / dodawanie komentarzy poprzez zaznaczenie linijki / linijek kodu i wykorzystanie skrótu CTRL + SHIFT + C

p <-  ggplot(new_df_1, aes(x = Date)) +
    geom_line(aes(y = model_Gasoline), size = 0.6, color = "#AC2B2D") +
    geom_line(aes(y = Gasoline), size = 0.9) +
    geom_hline(yintercept = 3, size = 1, colour="#333333") +
  # scale_x_date(breaks = as.Date(c("2011-01-01","2015-01-01", "2020-01-01", "2022-01-01")),
  #              labels = c("2011", "2015", "2020", "2022")) +
    scale_x_date(breaks = seq.Date(from = as.Date("2010-01-01"), to = as.Date("2022-01-01"), by = "2 years"),
                 date_labels = "%Y") +
  # xlim(c(as.Date("2009-06-01"), NA)) +
    coord_cartesian(xlim = c(as.Date("2010-01-01"), NA)) +
    bbc_style() +
    theme(
      plot.title = element_markdown(),
      plot.subtitle = element_markdown()) +
    labs(title = "Gasoline price in Poland goes up in line with the  brent oil price increase", 
         y = NULL, 
         subtitle = "What the gas price <span style='color:#AC2B2D;'>**should be**</span> based on brent oil price changes <span style='color:#111111;'>vs </span>**actual prices**") 
    
p

finalise_plot(plot_name = p,
              save_filepath = "Pb95_prices_in_Poland.png",
              width_pixels = 1000, #640 is default
              height_pixels = 550, #550 is default
              source = "Source: Own study")
```
