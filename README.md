# Polish_Gas_station_prices
Why does gas station prices for 95 oil is so high in Poland?


``` r
cena_paliwa <- read.csv("/Users/mr.fox/Desktop/Cena 95 w Polsce.csv", sep = ";", dec = ",") 
USD_PLN <- read_csv("/Users/mr.fox/Desktop/USD_PLN Historical.csv")
BRENT <- read_csv("/Users/mr.fox/Desktop/Brent Oil Futures Historical.csv")

USD_PLN <- USD_PLN %>% select(Date, Price) %>% mutate(Date = mdy(Date), USD_PLN = Price) %>% select(-Price)
BRENT <- BRENT %>% select(Date, Price) %>% mutate(Date = mdy(Date), BRENT = Price) %>% select(-Price)
cena_paliwa <- cena_paliwa %>% mutate(Date = dmy(Date)) %>% rename(Gasoil = Price)
ceny <- cena_paliwa %>%
  inner_join(USD_PLN, by = "Date") %>%
  inner_join(BRENT, by = "Date") %>%
  mutate(BRENT_PLN = BRENT * USD_PLN,
         stand_gasoil = (Gasoil - mean(Gasoil)) / sd(Gasoil),
         stand_BRENT_PLN = (BRENT_PLN - mean(BRENT_PLN)) / sd(BRENT_PLN),
         stand_BRENT = (BRENT - mean(BRENT)) / sd(BRENT))

p <- ggplot(ceny, aes(x = Date)) +
  geom_line(aes(y = stand_BRENT_PLN)) +
  geom_line(aes(y = stand_gasoil), color = "#AC2B2D") +
  geom_line(aes(y = stand_BRENT), color = "blue") +
  theme_classic() 

ggplotly(p)
```
