library(tidyverse) 
library(ggplot2)
library(dplyr)
library(rworldmap)
library(rworldxtra)
library(gganimate)
library(magick)
library(RColorBrewer)

map <- getMap()
plot(map)

#Source: Multiple sources compiled by World Bank (2024) – processed by Our World in Data
#https://ourworldindata.org/grapher/unemployment-rate?tab=chart#sources-and-processing


file_path<- "C:/Users/Annika/Documents/DBM2/Unemployment/unemployment-rate.csv"
file_path2 <- "C:/Users/Annika/Documents/DBM2/Unemployment/gdp.csv"
world_data <- read.csv(file_path)
gdp_data <- read.csv(file_path2)


#DATA PREPROCESSING
#1. GDP Data
head(gdp_data)
gdp_data <- gdp_data %>%
  rename(Country=Entity, GDP=Gross.domestic.product..GDP.)
gdp_data <- gdp_data %>% filter(Year >= 1991)
gdp_data <- gdp_data %>% select(-Code, -X900795.annotations)

#2. World Data
head(world_data)
colnames(world_data)
world_data <- world_data %>%
  rename(Unemployment_Rate=Unemployment..total....of.total.labor.force...modeled.ILO.estimate.,Country=Entity)
world_data <- world_data %>% 
  filter(Country != "World")

#1. Join World Data and GDP Data
world_data <- world_data %>%
  left_join(gdp_data, by = c("Country", "Year"))

#2. Missing Values
colSums(is.na(world_data))
na_gdp_countries <- merged_data %>% 
  filter(is.na(GDP)) %>% 
  select(Country) %>% 
  distinct()
print(na_gdp_countries)

#3. Add geo subregion as category (for countries for which there is no geo subregion: NA)

data("countryExData", package = "rworldmap")
country_continent <- unique(countryExData[, c("Country", "GEO_subregion")])
head(country_continent)
world_data <- merge(world_data, country_continent, by = "Country", all.x = TRUE)
colSums(is.na(world_data)) #63 -> thinking about solution (eventually adding manually)


#4. We have some rows in country that are NOT country so we divide this into three small datasets

region_data <- subset(world_data, Country %in% c("East Asia and Pacific (WB)","Europe and Central Asia (WB)",
"European Union (27)","Latin America and Caribbean (WB)","Middle East and North Africa (WB)",
                                               "North America (WB)","South Asia (WB)","Sub-Saharan Africa (WB)","World"))
region_data <- region_data %>%
  rename(Region=Country)

income <- subset(world_data, Country %in% c("High-income countries","Low-income countries",
                                            "Lower-middle-income countries","Middle-income countries",
                                            "Upper-middle-income countries"))

countries_to_delete <- c("High-income countries", "Low-income countries", "Lower-middle-income countries",
                         "Middle-income countries", "Upper-middle-income countries", "East Asia and Pacific (WB)",
                         "Europe and Central Asia (WB)", "European Union (27)", "Latin America and Caribbean (WB)",
                         "Middle East and North Africa (WB)", "North America (WB)", "South Asia (WB)",
                         "Sub-Saharan Africa (WB)", "World")

world_data <- world_data %>%
  filter(!Country %in% countries_to_delete)



#Now we have preprocessed data, reshaped data and added subregions, then we splitted the dataset into 3 
#sub datasets income where we can see the unemployment rate for low income, high income countries, and 
#unemployment for regions


#ANALYSIS without considering GDP

#Worldwide
#Income Analysis
ggplot(income, aes(x = Year, y = Unemployment_Rate, color = Country)) +
  geom_line() +
  labs(title = "Unemployment Rate Over Time by Country",
       x = "Year",
       y = "Unemployment Rate (%)",
       color = "Country") +
  theme_minimal()

#Analysis of regions using rather region subset or added regions (eventually use map to plot analysis)
#extract region that has biggest unemployment rate

#Country with highest unemployment then (1991) and now (2022)
highest_1991 <- world_data %>%
  filter(Year == 1991) %>%
  slice(order(Unemployment_Rate)) %>%
  tail(1) %>%
  pull(Country)

highest_2022 <- world_data %>%
  filter(Year == 2022) %>%
  slice(order(Unemployment_Rate)) %>%
  tail(1) %>%
  pull(Country)

filtered_data <- world_data %>%
  filter(Country %in% c(highest_1991, highest_2022))

ggplot(filtered_data, aes(x = Year, y = Unemployment_Rate, color = Country)) +
  geom_line() +
  labs(title = "Highest Unemployment Rate Over Time",
       x = "Year",
       y = "Unemployment Rate (%)",
       color = "Country") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, max(filtered_data$Unemployment_Rate) * 1.1))

#Add Italy for comparison
ggplot(world_data%>%
         filter(Country %in% c(highest_1991, highest_2022, "Italy")), aes(x = Year, y = Unemployment_Rate, color = Country)) +
  geom_line() +
  labs(title = "Highest Unemployment Rate Over Time in comparison with Italy",
       x = "Year",
       y = "Unemployment Rate (%)",
       color = "Country") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, max(filtered_data$Unemployment_Rate) * 1.1))


#Country with lowest unemployment then and now
lowest_1991 <- world_data %>%
  filter(Year == 1991) %>%
  arrange(Unemployment_Rate) %>%
  head(1) %>%
  pull(Country)

lowest_2022 <- world_data %>%
  filter(Year == 2022) %>%
  arrange(Unemployment_Rate) %>%
  head(1) %>%
  pull(Country)

filtered_data2 <- world_data %>%
  filter(Country %in% c(lowest_1991, lowest_2022))

ggplot(filtered_data2, aes(x = Year, y = Unemployment_Rate, color = Country)) +
  geom_line() +
  labs(title = "Lowest Unemployment Rate Over Time",
       x = "Year",
       y = "Unemployment Rate (%)",
       color = "Country") +
  theme_minimal() 

#Add Italy for comparison
ggplot(world_data%>%
         filter(Country %in% c(lowest_1991, lowest_2022, "Italy")), aes(x = Year, y = Unemployment_Rate, color = Country)) +
  geom_line() +
  labs(title = "Lowest Unemployment Rate Over Time in comparison with Italy",
       x = "Year",
       y = "Unemployment Rate (%)",
       color = "Country") +
  theme_minimal() 


#Changes in the Unemployment Rate
unemployment_change <- world_data %>%
  filter(Country != "Palestine", Country!="Montenegro") %>%
  group_by(Country) %>%
  summarise(
    Unemployment_Rate_Start = first(Unemployment_Rate[Year == min(Year)]),
    Unemployment_Rate_End = last(Unemployment_Rate[Year == max(Year)]),
    Change = Unemployment_Rate_End - Unemployment_Rate_Start
  )

#unemployment raised drastically
max_change <- max(unemployment_change$Change, na.rm = TRUE)

country_max_increase <- unemployment_change %>%
  filter(Change == max_change) %>%
  pull(Country)

country_data1 <- world_data %>%
  filter(Country == country_max_increase)

ggplot(country_data1, aes(x = Year, y = Unemployment_Rate)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = paste("Unemployment Rate Over Time -", country_max_increase),
       x = "Year",
       y = "Unemployment Rate (%)") +
  theme_minimal()

#unemployment unemployment sank drastically
min_change <- min(unemployment_change$Change, na.rm = TRUE)

country_min_decrease <- unemployment_change %>%
  filter(Change == min_change, Country!="Montenegro") %>%
  pull(Country)

country_data2 <- world_data %>%
  filter(Country == country_min_decrease)

ggplot(country_data2, aes(x = Year, y = Unemployment_Rate)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = paste("Unemployment Rate Over Time -", country_min_decrease),
       x = "Year",
       y = "Unemployment Rate (%)") +
  theme_minimal()


#Does region play a role in unemployment (region play roles with unemployment
#World
plot_map_for_year_w <- function(year_data, year) {
  # Join the data to the map
  map_data <- joinCountryData2Map(year_data, joinCode = "NAME", nameJoinColumn = "Country")
  
  # Plot the map
  map_plot <- mapCountryData(map_data, nameColumnToPlot = "Unemployment_Rate",
                             catMethod = "fixedWidth", mapRegion = "world",
                             mapTitle = paste("Unemployment Rates Worldwide -", year),
                             oceanCol = "lightblue", missingCountryCol = "white",
                             addLegend = TRUE)
  
  return(map_plot)
}
years <- unique(world_data$Year)
frames <- list()

for (year in years) {
  year_data <- world_data %>% filter(Year == year)
  frame <- plot_map_for_year_w(year_data, year)
  frames[[as.character(year)]] <- frame
}
png_files <- list.files(temp_dir, pattern = "frame_.*\\.png$", full.names = TRUE)

# Create GIF
animation <- image_read(png_files) %>%
  image_animate(fps = 1) %>%
  image_write("unemployment_animation_w.gif")


#Europe
# Plot the map

plot_map_for_year2 <- function(year_data, year) {
  # Join the data to the map
  map_data <- joinCountryData2Map(year_data, joinCode = "NAME", nameJoinColumn = "Country")
  
  # Plot the map
  map_plot <- mapCountryData(map_data, nameColumnToPlot = "Unemployment_Rate",
                             catMethod = "fixedWidth", mapRegion = "europe",
                             mapTitle = paste("Unemployment Rates in Europe -", year),
                             oceanCol = "lightblue", missingCountryCol = "white",
                             addLegend = TRUE)
  
  return(map_plot)
}
years <- unique(world_data$Year)
frames <- list()

for (year in years) {
  year_data <- world_data %>% filter(Year == year)
  frame <- plot_map_for_year2(year_data, year)
  frames[[as.character(year)]] <- frame
}
png_files <- list.files(temp_dir, pattern = "frame_.*\\.png$", full.names = TRUE)

# Create GIF
animation <- image_read(png_files) %>%
  image_animate(fps = 1) %>%
  image_write("unemployment_animation.gif")

#Take a look at several different countries, any year that has played a significant role for unemployment
#maybe some crisis? Hypothesis

#Italy vs other european countries
ggplot(world_data%>%
         filter(Country %in% c("Germany", "Spain","Sweden","Albania", "Italy")), aes(x = Year, y = Unemployment_Rate, color = Country)) +
  geom_line() +
  labs(title = "Unemployment Rate Over Time of other European countries in comparison with Italy",
       x = "Year",
       y = "Unemployment Rate (%)",
       color = "Country") +
  theme_minimal()




#ANALYSIS with GDP
#Is there a Correlation between GDP and unemployment rate? In which countries/years is the correlation
#the highest?


#Countries biggest GDP and lowest GDP
# Calculate average GDP for each country
average_gdp <- world_data %>%
  filter(!is.na(GDP)) %>%  # Filter out rows where GDP is NA
  group_by(Country) %>%
  summarize(avg_gdp = mean(GDP))
average_gdp <- average_gdp[order(average_gdp$avg_gdp, na.last = TRUE), ]

# Identify top 5 and bottom 5 countries by average GDP
bottom_countries <- head(average_gdp, 5)
top_countries <- tail(average_gdp, 5)

plot_avg_gdp <- function(countries, title) {
  ggplot(countries, aes(x = reorder(Country, avg_gdp), y = avg_gdp)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = title,
         x = "Country",
         y = "Average GDP") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Plot for top countries
plot_avg_gdp(top_countries, "Average GDP for Top 5 Countries")

# Plot for bottom countries
plot_avg_gdp(bottom_countries, "Average GDP for Bottom 5 Countries")






#GDP in World
plot_map_for_year_gdp <- function(year_data, year) {
  # Join the data to the map
  map_data <- joinCountryData2Map(year_data, joinCode = "NAME", nameJoinColumn = "Country")
  
  # Define breaks and labels for the legend
  breaks <- c(0, 1e10, 3e10, 1e11, 3e11, 1e12, 3e12, 1e13, 3e13)
  labels <- c("0", "10B", "30B", "100B", "300B", "1T", "3T", "10T")
  
  # Ensure that the number of colors matches the number of intervals
  num_colors <- length(breaks) -1
  
  # Categorize GDP data
  map_data$GDP_Cat <- cut(map_data$GDP, breaks = breaks, labels = labels, include.lowest = TRUE)
  
  # Plot the map
  map_plot <- mapCountryData(map_data, 
                             nameColumnToPlot = "GDP",
                             catMethod = "categorical",
                             mapRegion = "world",
                             mapTitle = paste("GDP Worldwide -", year),
                             colourPalette = brewer.pal(num_colors, "Blues"),
                             oceanCol = "lightblue", 
                             missingCountryCol = "white",
                             addLegend = FALSE)
  
  # Add custom legend
  addMapLegend(
    colourVector = brewer.pal(num_colors, "Blues"),
    cutVector = breaks,
    legendLabels = "all",
    labelFontSize = 0.7,
    legendWidth = 1.2,
    legendShrink = 0.9,
    legendMar = 3,
    horizontal = TRUE,
    legendArgs = list(text = "GDP (in USD)", side = 1, line = 3, cex = 1),
    tcl = -0.5,
    mgp = c(3, 1, 0),
    sigFigs = 4,
    digits = 3,
    legendIntervals = "page"
  )
  
  return(map_plot)
}

years <- unique(world_data$Year)
frames <- list()

for (year in years) {
  year_data <- world_data %>% filter(Year == year)
  frame <- plot_map_for_year_gdp(year_data, year)
  frames[[as.character(year)]] <- frame
}
png_files <- list.files(temp_dir, pattern = "frame_.*\\.png$", full.names = TRUE)

# Create GIF
animation <- image_read(png_files) %>%
  image_animate(fps = 1) %>%
  image_write("gdp_animation.gif")


#GDP Europe
plot_map_for_year_gdp_e <- function(year_data, year) {
  # Join the data to the map
  map_data <- joinCountryData2Map(year_data, joinCode = "NAME", nameJoinColumn = "Country")
  
  # Define breaks and labels for the legend
  breaks <- c(0, 1e10, 3e10, 1e11, 3e11, 1e12, 3e12, 1e13, 3e13)
  labels <- c("0", "10B", "30B", "100B", "300B", "1T", "3T", "10T")
  
  # Ensure that the number of colors matches the number of intervals
  num_colors <- length(breaks) - 1
  
  # Categorize GDP data
  map_data$GDP_Cat <- cut(map_data$GDP, breaks = breaks, labels = labels, include.lowest = TRUE)
  
  # Plot the map for Europe
  map_plot <- mapCountryData(map_data, 
                             nameColumnToPlot = "GDP",
                             catMethod = "categorical",
                             mapRegion = "Europe",
                             mapTitle = paste("GDP in Europe -", year),
                             colourPalette = brewer.pal(num_colors, "Blues"),
                             oceanCol = "lightblue", 
                             missingCountryCol = "white",
                             addLegend = FALSE)
  
  # Add custom legend
  addMapLegend(
    colourVector = brewer.pal(num_colors, "Blues"),
    cutVector = breaks,
    legendLabels = "all",
    labelFontSize = 0.7,
    legendWidth = 1.2,
    legendShrink = 0.9,
    legendMar = 3,
    horizontal = TRUE,
    legendArgs = list(text = "GDP (in USD)", side = 1, line = 3, cex = 1),
    tcl = -0.5,
    mgp = c(3, 1, 0),
    sigFigs = 4,
    digits = 3,
    legendIntervals = "page"
  )
  
  return(map_plot)
}

years <- unique(world_data$Year)
frames <- list()

for (year in years) {
  year_data <- world_data %>% filter(Year == year)
  frame <- plot_map_for_year_gdp_e(year_data, year)
  frames[[as.character(year)]] <- frame
}

# Create GIF
png_files <- list.files(temp_dir, pattern = "frame_.*\\.png$", full.names = TRUE)

animation <- image_read(png_files) %>%
  image_animate(fps = 1) %>%
  image_write("gdp_europe_animation.gif")



#Okuns law: a percentage in unemployment, two percent fall in GDP
distinct_subregions <- world_data %>%
  distinct(GEO_subregion) %>%
  arrange(GEO_subregion)

#GDP change several countries worldwide
ggplot(world_data%>%
         filter(Country %in% c("Germany", "United States","China","South Africa", "Italy")), aes(x = Year, y = GDP, color = Country)) +
  geom_line() +
  labs(title = "GDP Over Time of other European countries in comparison with Italy",
       x = "Year",
       y = "GDP ($)",
       color = "Country") +
  theme_minimal()







#Regions GDP vs Unemployment rate

# Function to calculate year-over-year change GD, Unemployment Rate
calculate_yoy_change_percent <- function(data) {
  data <- data %>%
    mutate(
      GDP_Change_Percent = if_else(Year == 1991 | is.na(GDP), NA_real_, (GDP / lag(GDP) - 1) * 100),
      Unemployment_Rate_Change_Percent = if_else(Year == 1991 | is.na(Unemployment_Rate), NA_real_, (Unemployment_Rate / lag(Unemployment_Rate) - 1) * 100)
    )
  return(data)
}
# Apply the function for each country
world_data <- world_data %>%
  group_by(Country) %>%
  arrange(Year) %>%
  group_modify(~ calculate_yoy_change_percent(.))


#For a specific country we plot the changes
c_data <- world_data %>%
  filter(Country == "Algeria")

# Plot GDP and Unemployment Rate Change Percentages in the same plot
ggplot(c_data, aes(x = Year)) +
  geom_line(aes(y = GDP_Change_Percent, color = "GDP Change"), size = 1.2) +
  geom_line(aes(y = Unemployment_Rate_Change_Percent, color = "Unemployment Rate Change"), linetype = "dashed", size = 1.2) +
  labs(title = "Annual Percentage Change in GDP and Unemployment Rate",
       x = "Year",
       y = "Change (%)",
       color = "Variable") +
  scale_color_manual(values = c("GDP Change" = "blue", "Unemployment Rate Change" = "red")) +
  theme_minimal()
  

#Correlations
# Filter out rows with missing values in GDP or Unemployment Rate
clean_data <- world_data %>%
  filter(complete.cases(GDP, Unemployment_Rate))

# Calculate correlation matrix
correlation_matrix <- clean_data %>%
  group_by(Country) %>%
  summarise(correlation = cor(GDP, Unemployment_Rate))

# View correlation matrix
print(correlation_matrix)
