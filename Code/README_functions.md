# Functions and code 

During creation of the analysis some code was reshaped into separate functions. They can be found in a folder functions. As much as I could I developed some steps that would help you in creating the correct data structure. You can find a way some of the plots where created in visualisation code which use these functions. Here I will briefly describe the possibilities and limits of the functions.


## Univariate map EU

![map](https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Inflation%20HICP%20in%20Europe.png "Inflation HICP in Europe")

This function creates a univariate map for Europe countries. The arguments that need to be provided: 
- data - data that will be used. The color scale is from yellow for the lowest values to dark purple for the highest values. You can change it in a function details in a direction argument.

Optional arguments to set the plot elements:
- title
- subtitle
- caption
- legend

Optional argument to work on it:
- annotation - if changed to TRUE this enables the function to save a plot in a global environment under a name 'univariate_plot'. I recommend using ggannotate package and its ggannotate function to add comments, additional content on a plot.

```R
p1 <- univariate_map_eu(data = provided_data)
ggsave("uniariate_map_eu.png", p1, dpi = 900, height = 15, width = 22, units = "cm")
```


## Bivariate map EU

![map](https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Inflation%20and%20share%20of%20YoY%20increase%20in%20energy%20and%20food%20prices%20in%20HICP%20inflation%20in%20Europe.png "Inflation and share of YoY increase in energy and food prices in HICP inflation in Europe")

This function creates a bivariate map for Europe countries. The arguments that need to be provided: 
- data_x - data that will be used on x axis on the legend. It's color is red
- data_y - data that will be used on y axis on the legend. It's color is blue

Optional arguments to set the plot elements:
- title
- subtitle
- caption
- legend_x
- legend_y

Optional argument to work on it:
annotation - if changed to TRUE this enables the function to save a plot in a global environment under a name 'bivariate_plot'. I recommend using ggannotate package and its ggannotate function to add comments, additional content on a plot.


```R
p2 <- bivariate_map_eu(data_x = provided_data_to_x, data_y = provided_data_to_y)
ggsave("bivariate_map_eu.png", p2, dpi = 900, height = 15, width = 22, units = "cm")
```


## Inflation chart

![chart](https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Inflation%20in%20selected%20countries.png "Inflation in selected countries")

This function creates an inflation chart for Europe countries. There are no arguments that need to be provided. The function works on Eurostat database.

Optional arguments to set the plot elements:
- rate_of_change - two options: 'annual' for annual inflation and 'monthly' for monthly inflation.
- region - default option is set to 'All'. This displays all the inflation for all the countries available in eurostat database. You can change it by typing countries name, code or both.
- block_region - default option is set to 'TR' for Turkey as that country has a very big inflation that makes it difficult to differentiate other countries. In case you want to display Turkey in region change block_region to other, unused country.
- from_date - default option is '2019-01-01'. In order to change it type different date in 'YYYY-MM-DD' format.
- language_output - default language is 'en' for english. There is a possibility to change it to 'pl' for polish one.


```R
p3 <- inflation_chart()

# For the plot with all regions height = 50 and width = 40 cm dimensions are recommended.
ggsave("inflation_chart_all.png", p3, dpi = 900, height = 50, width = 40, units = "cm")
```

```R
p4 <- inflation_chart(region = c("Austria", "Malta", "DE", "PL", "Hungary"), language_output = "pl")

# For the plot with 5 regions height = 15 and width = 22 cm dimensions are recommended.
ggsave("inflation_chart_selected.png", p4, dpi = 900, height = 15, width = 22, units = "cm")
```