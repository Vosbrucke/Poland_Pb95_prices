library(rstudioapi)
library(tidyverse) 
library(magrittr) 
library(sf) 
library(viridis)
library(cowplot)
library(rmarkdown)
library(eurostat)

# Plot a Univariate Choropleth
univariate_map_eu <- function(data, title = "", subtitle = "", legend = "", caption = "", annotation = FALSE) {
  # Function: Szymon Lisowski
  # Inspired by: Timo Gressenbacher and Angelo Zehr
  
  # Interrupt if provided data frame does not have 2 columns
  if (length(colnames(data)) != 2) {
    stop("Provided data frame to data does not contain two columns. Limit the number of columns to one containing geo code named as 'geo' and the second with values named as 'values_x'")
  }
  
  # Interrupt if provided data frame to x does not contain proper column names
  if (sum(colnames(data) == "values_x") != 1) {
    stop("There is no 'values_x' column in data_x data frame. Change the values column name to 'values_x'")
  } else if ((sum(colnames(data) == "geo") != 1)) {
    stop("There is no 'geo' column in data_x data frame. Change the location, countries code column name to 'geo'")
  }
  
  if (nrow(data) > 60) {
    stop("There are too many observations in the data frame. Filter the observations to maximum 1 per country")
  }
  
  # The code, with some changes, is derived from Timo Grossenbacher's blog: https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/#outline
  # I highly recommend checking instructions- every step in the process of creating this kind of plot is described.
  # Huge thanks to the authors of the instruction: Timo Gressenbacher and Angelo Zehr.
  
  # define number of classes
  no_classes <- 6
  
  # extract quantiles
  quantiles <- data %>%
    filter(values_x != 0) %>%
    pull(mean(values_x)) %>%
    quantile(probs = seq(0, 1, length.out = no_classes + 1)) %>%
    as.vector() # to remove names of quantiles, so idx below is numeric
  
  if (sum(quantiles >= 10) > no_classes / 3) {
    round_n <- 0
    nsmall <- 0
  } else {
    round_n <- 2
    nsmall <- 2
  }
  
  # here we create custom labels
  labels <- purrr::imap_chr(quantiles, function(., idx){
    return(paste0(format(round(quantiles[idx], round_n), nsmall = nsmall),
                  " – ",
                  format(round(quantiles[idx + 1], round_n), nsmall = nsmall)
    ))
  })
  
  # we need to remove the last label 
  labels <- labels[1:length(labels) - 1]
  
  # here we actually create a new 
  # variable on the dataset with the quantiles
  data %<>%
    mutate(mean_quantiles = cut(values_x,
                                breaks = quantiles,
                                labels = labels,
                                include.lowest = T))
  
  # For quick change in background color
  background_color <- "#F5F5F2" # "white" or the original color "#F5F5F2"
  
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
        plot.background = element_rect(fill = background_color,
                                       color = NA),
        panel.background = element_rect(fill = background_color,
                                        color = NA),
        legend.background = element_rect(fill = background_color,
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
        plot.caption.position = "plot",
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
  
  SHP_27 <- SHP_0 %>% 
    select(geo, geometry) %>% 
    inner_join(data, by = "geo") %>% 
    arrange(geo) %>% 
    st_as_sf()
  
  p <- SHP_27 %>%
    ggplot(
    ) +
    geom_sf(
      data = SHP_0,
      aes(),
      fill = "white",
      color = "white",
      size = 0.4,
      alpha = 0
    ) +
    geom_sf(
      aes(),
      color = "white",
      size = 0.4
    ) +
    geom_sf(
      aes(fill = mean_quantiles),
      color = "white",
      size = 0.4,
      alpha = 0.8
    ) +
    viridis::scale_fill_viridis(
      option = "magma",
      name = legend,
      alpha = 0.8,
      begin = 0.1, # this option seems to be new (compared to 2016):
      # with this we can truncate the
      # color scale, so that extreme colors (very dark and very bright) are not
      # used, which makes the map a bit more aesthetic
      end = 0.9,
      direction = -1,
      discrete = T, # discrete classes, thus guide_legend instead of _colorbar
      # direction = 1, # dark is lowest, yellow is highest
      guide = guide_legend(
        keyheight = unit(5, units = "mm"),
        title.position = "top",
        reverse = T # display highest value on top
      )) +
    coord_sf(xlim = c(-24, 45), ylim = c(34, 73), expand = FALSE) +
    labs(x = NULL,
         y = NULL,
         title = title,
         subtitle = subtitle,
         caption = caption) +
    theme_map()

  if (annotation) {
    warning("Under name 'univariate_plot' a plot is saved as ggplot object. You can add annotations to it or make any other changes. There may be a risk of overriding certain settings which can result in a missbehaving plot")
    assign("univariate_plot", p)
  } else {
    p
  }
}
