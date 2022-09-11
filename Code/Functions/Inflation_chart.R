library(eurostat)
library(tidyverse)
library(metR)
library(lubridate)

inflation_chart <- function(rate_of_change = "annual", region = "All", block_region = "TR", from_date = "2019-01-01", language_output = "en") {
  # Function author: Szymon Lisowski
  # Inspired by: Franz X. Mohr, 'Decomposing Inflation for EA Countries'
  
  # Read country names to plot
  countries_eu <- read.csv("Processed_data/countries_eu_inflation.csv", sep = ",")
  
  # Constants
  possible_rate_of_change <- c("annual", "monthly")
  
  possible_country_names <- countries_eu$country_name
  stop_countries_names <- paste(possible_country_names, collapse = "', '")
  
  possible_country_codes <- countries_eu$code
  stop_countries_code <- paste(possible_country_codes, collapse = "', '")
  
  possible_language_output <- c("en", "pl")
  
  # Check if rate of change argument is correct
  if (sum(rate_of_change %in% possible_rate_of_change) != 1) {
    stop("Provided inflation rate of change is incorrect. Use 'year' to get annual rate of change or 'monthly' for monthly rate of change.")
  }
  
  # Check if region argument is correct
  if (sum(region != "All") > 0) {
    # Check if given region is possible to plot
    if (sum(!as.character(region) %in% c(possible_country_names, possible_country_codes)) != 0) {
      # Throw an error that there is no such country or code found
      stop(paste0("Provided region code is not correct. Use 'All' or vector of the following ones: '", stop_countries_names,"'. You can also use geo codes: '", stop_countries_code, "'."))
    } else {
      # Make a data frame with given regions if not plotting 'All'
      region <- data.frame(code = region, country_name = region)
      
      region <- bind_rows(countries_eu %>% semi_join(region, by = "code"), 
                          countries_eu %>% semi_join(region, by = "country_name"))
    }
  } else {
    region <- data.frame(code = "All")
  }
  
  # Check if block_region argument is correct
  if (sum(!block_region %in% c(possible_country_names, possible_country_codes)) > 0) {
    stop(paste0("Provided region code is not correct. Use 'All' or vector of the following ones: '", stop_countries_names,"'. You can also use geo codes: '", stop_countries_code, "'."))
  } else {
    # Make a data frame with given block regions if not plotting 'All'
    block_region <- data.frame(code = block_region, country_name = block_region)
    
    block_region <- bind_rows(countries_eu %>% semi_join(block_region, by = "code"), 
                              countries_eu %>% semi_join(block_region, by = "country_name"))
    
  }
  
  if (nrow(region %>% semi_join(block_region, by = "code")) > 0) {
    stop(paste0("Provided region and block region contain one or more of the same region code. Change one of the function argument values."))
  }
  
  
  # Check if the language output argument is correct
  if (sum(!language_output %in% possible_language_output) > 0) {
    stop("Provided language output is not correct. Use 'en' to make a plot in english or 'pl' to make a plot in polish.")
  }
  
  # Catch error if from date argument is incorrect and stop the function
  if_error <- tryCatch(as.Date(from_date), error = function(i) i)
  
  if (any(class(if_error) == "error") == TRUE || !is.Date(as.Date(from_date))) {
    stop("This argument value for 'from_date' is not available! Format date as 'YYYY-MM-DD' instead.")
  }
  
  # Rate of change assignment 
  if (rate_of_change == "annual") {
    id <- "prc_hicp_manr"
  } else {
    id <- "prc_hicp_mmor"
  }
  
  # Downloading and adjusting data from eurostat
  inflation <- get_eurostat(id = id,
                            filters = list(coicop = c("CP00", "TOT_X_NRG_FOOD", "FOOD", "NRG", "IGD_NNRG", "SERV"))) %>% 
    filter(!geo %in% c("EU28", "EU27_2020", "EA18", "UK", "US", "EEA")) %>% 
    filter(time >= as.Date(from_date) & !is.na(values)) %>%
    mutate(year = lubridate::year(time))
  
  # Filter inflation for given regions
  if (sum(region %in% "All") != 1) {
    inflation <- inflation %>%
      filter(geo %in% region$code)
  }
  
  # Make a correct language output for parts of the plot
  if (language_output[1] == "pl") {
    # Polish language
    country_code_name_binned <- countries_eu %>% select(-country_name) %>% rename(country_name = country_name_pl)
    
    labels_color <- c("Usługi", "Nieenergetyczne towary przemysłowe",
                      "Energia", "Żywność, w tym alkohol i tytoń")
    labels_line <- c("Inflacja HICP", "Inflacja bazowa (z wyłączeniem energii i żywności)")
    title <- paste("Udział poszczególnych czynników w", ifelse(rate_of_change == "annual", "rocznej", "miesięcznej"), "inflacji")
    caption <- "Źródło: Eurostat\nCC BY"
  } else {
    # English language
    country_code_name_binned <- countries_eu %>% select(-country_name_pl)
    
    labels_color <- c("Services", "Non-energy industrial goods",
                      "Energy", "Food including alcohol and tobacco")
    labels_line <- c("HCPI-inflation", "Core inflation (w/o energy, food)")
    caption <- "Source: Eurostat\nCC BY"
  }
  
  # Filter inflation by blocked region
  inflation <- inflation %>% 
    left_join(country_code_name_binned, by = c("geo" = "code")) %>% 
    filter(!geo %in% block_region$code)
  
  # Downloading and adjusting weights to inflation coicop categories
  weights <- get_eurostat(id = "prc_hicp_inw",
                          filters = list(coicop = c("FOOD", "NRG", "IGD_NNRG", "SERV"))) %>% 
    mutate(year = lubridate::year(time), values = values / 10) %>%
    select(-time) %>%
    rename(weight = values)
  
  # Join inflation and weights. 
  index <- inflation %>% 
    filter(!coicop %in% c("CP00", "TOT_X_NRG_FOOD")) %>%
    left_join(weights, by = c("year", "geo", "coicop")) %>%
    # Obtain values for composition
    group_by(time, geo) %>%
    mutate(weight = weight / sum(weight),
           values = values * weight) %>%
    ungroup() %>%
    # Last formatting for graph
    mutate(var_en = factor(coicop, levels = c("SERV", "IGD_NNRG", "NRG", "FOOD"),
                           labels = labels_color))
  
  # Make a data frame for line plot
  line <- inflation %>%
    # Filter for series of the line plots
    filter(coicop %in% c("CP00", "TOT_X_NRG_FOOD"),
           !is.na(values)) %>%
    # Last formatting for graph
    mutate(line = factor(coicop, levels = c("CP00", "TOT_X_NRG_FOOD"),
                         labels = labels_line))
  
  # Prepare breaks to adjust labels in the plot  
  breaks <- metR::MakeBreaks(binwidth = NULL, bins = 4, exclude = NULL)
  breaks <- breaks(line$values)
  
  # Make a palet of colors
  palette <- wesanderson::wes_palette("Zissou1", n = 4, type = "continuous")
  
  # Plot
  p <- ggplot(index, aes(x = time, y = values)) +
    geom_col(aes(fill = var_en)) +
    geom_line(data = line, aes(linetype = line), size = 0.75) +
    geom_hline(aes(yintercept = 0)) +
    scale_fill_manual(values = palette) +
    scale_x_date(expand = c(.01, 0), date_labels = "%m %Y") +
    scale_y_continuous(labels = paste0(breaks, "%"), breaks = breaks) +
    guides(linetype = guide_legend(ncol = 2, keywidth = 1.5),
           fill = guide_legend(ncol = 2)) +
    labs(title = title, 
         caption = caption,
         x = "", y = "") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.y = element_line(size = 0.2, color = "lightgrey"),
          plot.title = element_text(face = "bold"),
          plot.caption = element_text(hjust = 0, size = 4))
  
  # Finalise plot by wrapping if there is more than one region given
  if (length(region$code) > 1 || region$code == "All") {
    p <- p +   
      facet_wrap(~ country_name, ncol = 5)
  } else {
    p <- p +
      labs(title = ifelse(language_output == "pl", 
                          paste0(inflation$country_name %>% unique(), ", udział poszczególnych czynników w ", ifelse(rate_of_change == "annual", "rocznej", "miesięcznej"), " inflacji"),
                          paste0(inflation$country_name %>% unique(), ", share of individual factors in ", ifelse(rate_of_change == "annual", "annual", "monthly"), " inflation")
                          
      ))
  }
  # Final plot
  p
}