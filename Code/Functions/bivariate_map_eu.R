library(rstudioapi)
library(tidyverse) 
library(magrittr) 
library(sf) 
library(viridis)
library(cowplot)
library(rmarkdown)
library(eurostat)

# Plot a Bivariate Choropleth
bivariate_map_eu <- function(data_x = data_x, data_y = data_y, title = "", subtitle = "", legend_x = "values_x", legend_y = "values_y", caption = "", annotation = FALSE) {
  # Function author: Szymon Lisowski
  # Inspired by: Timo Gressenbacher and Angelo Zehr
  
  # Interrupt if provided data frame to x does not have 2 columns
  if (length(colnames(data_x)) != 2) {
    stop("Provided data frame to data_x does not contain two columns. Limit the number of columns to one containing geo code named as 'geo' and the second with values named as 'values_x'")
  }
  
  # Interrupt if provided data frame to x does not contain proper column names
  if (sum(colnames(data_x) == "values_x") != 1) {
    stop("There is no 'values_x' column in data_x data frame. Change the values column name to 'values_x'")
  } else if ((sum(colnames(data_x) == "geo") != 1)) {
    stop("There is no 'geo' column in data_x data frame. Change the location, countries code column name to 'geo'")
  }
  
  # Interrupt if provided data frame to y does not have 2 columns
  if (length(colnames(data_y)) != 2) {
    stop("Provided data frame to data_y does not contain two columns. Limit the number of columns to one containing geo code named as 'geo' and the second with values named as 'values_y'")
  }
  
  # Interrupt if provided data frame to x does not contain proper column names
  if (sum(colnames(data_y) == "values_y") != 1) {
    stop("There is no 'values_y' column in data_y data frame. Change the values column name to 'values_y'")
  } else if ((sum(colnames(data_x) == "geo") != 1)) {
    stop("There is no 'geo' column in data_y data frame. Change the location, countries code column name to 'geo'")
  }
  
  if (sum(c(nrow(data_y), nrow(data_x)) > 60) != 0) {
    stop("There are too many observations in one of provided data frames. Filter the observations to maximum 1 per country")
  }
  
  # Drop na observations
  data_x <- data_x %>%
    drop_na()
  data_y <- data_y %>% 
    drop_na()
  
  # The code, with some changes, is derived from Timo Grossenbacher's blog: https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/#outline
  # I highly recommend checking instructions where every step in the process of creating this kind of plot is described.
  # Huge thanks to the authors of the instruction: Timo Gressenbacher and Angelo Zehr.
  
  # create 3 buckets for data on x axis
  quantiles_x<- data_x %>%
    pull(values_x) %>%
    quantile(probs = seq(0, 1, length.out = 4))
  
  # create 3 buckets for data on y axis
  quantiles_y <- data_y %>%
    pull(values_y) %>%
    quantile(probs = seq(0, 1, length.out = 4))
  
  # create color scale that encodes two variables
  # red for gini and blue for mean income
  # the special notation with gather is due to readibility reasons
  bivariate_color_scale <- tibble(
    "3 - 3" = "#3F2949", # high inequality, high income
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low inequality, high income
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium inequality, medium income
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high inequality, low income
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low inequality, low income
  ) %>%
    gather("group", "fill")
  
  data <- data_x %>%
    inner_join(data_y, by = "geo")
  
  # cut into groups defined above and join fill
  data <- data %>%
    mutate(
      quantiles_x = cut(
        values_x,
        breaks = quantiles_x,
        include.lowest = TRUE
      ),
      quantiles_y = cut(
        values_y,
        breaks = quantiles_y,
        include.lowest = TRUE
      ),
      # by pasting the factors together as numbers we match the groups defined
      # in the tibble bivariate_color_scale
      group = paste(
        as.numeric(quantiles_x), "-",
        as.numeric(quantiles_y)
      )
    ) %>%
    # we now join the actual hex values per "group"
    # so each municipality knows its hex value based on the his gini and avg
    # income value
    left_join(bivariate_color_scale, by = "group")
  
  
  # separate the groups
  bivariate_color_scale %<>%
    separate(group, into = c("values_x", "values_y"), sep = " - ") %>%
    mutate(values_x = as.integer(values_x),
           values_y = as.integer(values_y))
  
  # Add a theme function
  theme_map <- function(...) {
    theme_minimal() +
      theme(
        # remove all axes
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        # add a subtle grid
        panel.grid.major = element_line(color = "#ECECE9", size = 0.2),
        panel.grid.minor = element_blank(),
        # background colors
        plot.background = element_rect(fill = "#F5F5F2",
                                       color = NA),
        panel.background = element_rect(fill = "#F5F5F2",
                                        color = NA),
        legend.background = element_rect(fill = "#F5F5F2",
                                         color = NA),
        # borders and margins
        plot.margin = unit(c(.5, .5, .2, .5), "cm"),
        panel.border = element_blank(),
        panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
        # titles
        legend.title = element_text(size = 11, color = "#494E4F"),
        legend.text = element_text(size = 9, hjust = 0,
                                   color = "#494E4F"),
        plot.title = element_text(size = 15, hjust = 0.5,
                                  color = "#494E4F"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 10, hjust = 0.5,
                                     color = "#494E4F",
                                     margin = margin(b = -0.1,
                                                     t = -0.1,
                                                     l = 2,
                                                     unit = "cm"),
                                     debug = F),
        # captions
        plot.caption = element_text(size = 7,
                                    hjust = .5,
                                    margin = margin(t = 0.2,
                                                    b = 0,
                                                    unit = "cm"),
                                    color = "#939184"),
        ...
      )
  }
  
  # Read shapefile for Europe
  SHP_0 <- get_eurostat_geospatial(resolution = 10, nuts_level = 0, year = 2021, output_class = "sf", make_valid = FALSE)
  
  # Join shapefile with data 
  SHP_27 <- SHP_0 %>% 
    select(geo = NUTS_ID, geometry) %>% 
    inner_join(data, by = "geo") %>% 
    arrange(geo) %>% 
    st_as_sf()
  
  # Plot a map
  map <- ggplot(
    data = SHP_27
  ) +
    scale_alpha(name = "",
                range = c(0.6, 0),
                guide = "none") + # suppress legend
    geom_sf(data = SHP_0,
            aes(),
            fill = "white",
            color = "white",
            size = 0.4,
            alpha = 0
    ) +
    # Make outlines of countries where data is not available
    geom_sf(
      aes(),
      color = "white",
      size = 0.4,
    ) +
    geom_sf(
      aes(
        fill = fill
      ),
      color = "white",
      size = 0.4,
    ) +
    scale_fill_identity() +
    coord_sf(xlim = c(-24, 45), ylim = c(34, 73), expand = FALSE) +
    labs(x = NULL,
         y = NULL,
         title = title,
         subtitle = subtitle,
         caption = caption) +
    theme_map()
  
  # Plot a legend
  legend <- ggplot() +
    geom_tile(
      data = bivariate_color_scale,
      mapping = aes(
        x = values_y,
        y = values_x),
      fill = "white"
    ) +
    geom_tile(
      data = bivariate_color_scale,
      mapping = aes(
        x = values_x,
        y = values_y,
        fill = fill),
      alpha = 1
    ) +
    # Here I wanted to keep information about the scope of the value. 
    # Using geom_text I added information on two breaks in the quantiles
    geom_text(aes(x = c(1.5, 2.5), y = c(3.7, 3.7), label = c(round(quantiles_x[2]), round(quantiles_x[3]))), size = 1, color = "#494E4F") +
    geom_text(aes(x = c(3.7, 3.7), y = c(1.5, 2.5), label = c(round(quantiles_y[2]), round(quantiles_y[3]))), size = 1, color = "#494E4F") +
    scale_fill_identity() +
    labs(x = legend_x,
         y = legend_y) +
    scale_x_continuous(limits = c(0, 4), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, 4), expand = c(0,0)) +
    theme_map() +
    theme(
      panel.border = element_blank(),
      panel.spacing = element_blank(),
      axis.title = element_text(size = 6, color = "#494E4F"),
      panel.grid.major = element_blank()
    ) +
    coord_fixed()
  
  # Drawing two plots together on one plot
  p <- ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0, 0, 0.25, 0.25) +
    theme(plot.background = element_rect(fill = "#F5F5F2", color = "#F5F5F2"))
  
  if (annotation) {
    warning("Under name 'bivariate_plot' a plot is saved as ggplot object. You can add annotations to it or make any other changes. There may be a risk of overriding certain settings which can result in a missbehaving plot")
    assign("bivariate_plot", p, envir = .GlobalEnv)
  } else {
    p
  }
}