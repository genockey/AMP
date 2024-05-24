## install packages 

if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("rlang")) install.packages("rlang", dependencies = TRUE)
if (!require("gridExtra")) install.packages("gridExtra", dependencies = TRUE)

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(rlang) 
library(gridExtra)

# Read the data
air_quality_data <- read.csv("WHO-air-quality.csv")

# Define the cities of interest
cities_of_interest <- c("Washington Arlington Alexandria DC VA MD WV/USA",
                        "London/GBR", "Paris/FRA", "Barcelona/ESP", 
                        "Berlin/DEU", "Tokyo/JPN", "Melbourne/AUS")

# Filter the data for the specified cities
filtered_data <- filter(air_quality_data, city %in% cities_of_interest)

# Function to plot trends for a given pollutant
plot_pollutant_trends <- function(data, pollutant) {
  g <- ggplot(data = data, aes(x = year, y = .data[[pollutant]], group = city, color = city)) +
    geom_line(na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    scale_color_discrete(name = "City", labels = sapply(strsplit(cities_of_interest, "/"), `[`, 1)) +
    labs(title = paste("Trends in", gsub("_", " ", pollutant)),
         x = "Year",
         y = paste(gsub("_", " ", pollutant), " (µg/m³)")) +
    theme_minimal() +
    theme(axis.text = element_text(color = "black"),
          axis.title = element_text(color = "black", face = "bold"),
          plot.title = element_text(face = "bold"))
  return(g)
}

# Generate plots for PM10, PM2.5, and NO2
plot_pm10 <- plot_pollutant_trends(filtered_data, "pm10_concentration")
plot_pm25 <- plot_pollutant_trends(filtered_data, "pm25_concentration")
plot_no2 <- plot_pollutant_trends(filtered_data, "no2_concentration")

# Combine the plots into a single image
combined_plot <- grid.arrange(plot_pm10, plot_pm25, plot_no2, ncol = 1, nrow = 3)
print(combined_plot)

## Descriptive statistics
# Descriptive statistics for PM2.5
descriptive_stats_pm25 <- filtered_data %>%
  group_by(city) %>%
  summarise(
    count = n(),
    mean_pm25 = mean(pm25_concentration, na.rm = TRUE),
    median_pm25 = median(pm25_concentration, na.rm = TRUE),
    sd_pm25 = sd(pm25_concentration, na.rm = TRUE),
    min_pm25 = min(pm25_concentration, na.rm = TRUE),
    max_pm25 = max(pm25_concentration, na.rm = TRUE)
  )

# Print the descriptive statistics
print(descriptive_stats_pm25)

# Trend analysis function for PM2.5
perform_trend_analysis <- function(data, pollutant) {
  trend_results <- list()
  for (city in cities_of_interest) {
    city_data <- filter(data, city == !!city)
    trend <- lm(reformulate("year", pollutant), data = city_data)
    trend_summary <- summary(trend)
    trend_results[[city]] <- list(
      slope = coef(trend)[2],
      p_value = coef(trend_summary)[2, 4]
    )
  }
  return(trend_results)
}

# Trend analysis for PM2.5
pm25_trends <- perform_trend_analysis(filtered_data, "pm25_concentration")
print(pm25_trends)

# ANOVA test for PM2.5
anova_pm25 <- aov(pm25_concentration ~ city, data = filtered_data)
summary(anova_pm25)

# Conduct Tukey's HSD test
tukey_hsd <- TukeyHSD(anova_pm25)

# Print the results
print(tukey_hsd)


# Function to plot trends for PM2.5
plot_pollutant_trends <- function(data, pollutant) {
  g <- ggplot(data = data, aes(x = year, y = .data[[pollutant]], group = city, color = city)) +
    geom_line(na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    scale_color_discrete(name = "City", labels = sapply(strsplit(cities_of_interest, "/"), `[`, 1)) +
    labs(title = paste("Trends in", gsub("_", " ", pollutant)),
         x = "Year",
         y = paste(gsub("_", " ", pollutant), " (µg/m³)")) +
    theme_minimal() +
    theme(axis.text = element_text(color = "black"),
          axis.title = element_text(color = "black", face = "bold"),
          plot.title = element_text(face = "bold"))
  return(g)
}

# Plot trends for PM2.5
plot_pm2.5 <- plot_pollutant_trends(filtered_data, "pm25_concentration")

# Display the plot
print(plot_pm2.5)

## Looking at deaths attributable to air pollution 
# Load necessary libraries
library(ggplot2)

# Create the data frame
data <- data.frame(
  Country = c("UK", "France", "Germany", "Australia", "Japan", "USA", "Spain"),
  Deaths_Attributable_to_Air_Pollution = c(6.1, 5.9, 6.2, 4.0, 6.4, 7.1, 5.7)
)

# Plot the data
# Load necessary libraries
library(ggplot2)

# Create the data frame
data <- data.frame(
  Country = c("UK", "France", "Germany", "Australia", "Japan", "USA", "Spain"),
  Deaths_Attributable_to_Air_Pollution = c(6.1, 5.9, 6.2, 4.0, 6.4, 7.1, 5.7)
)

# Sort the data by Deaths_Attributable_to_Air_Pollution in descending order
data <- data[order(-data$Deaths_Attributable_to_Air_Pollution), ]

# Plot the data
ggplot(data, aes(x = reorder(Country, Deaths_Attributable_to_Air_Pollution), y = Deaths_Attributable_to_Air_Pollution, fill = Country)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = Deaths_Attributable_to_Air_Pollution), hjust = -0.3, size = 5) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  labs(
    title = "Percentage of Total Deaths Attributable to Air Pollution (2019)",
    subtitle = "Source: WHO and Our World in Data",
    x = "Country",
    y = "Percentage of Total Deaths Attributable to Air Pollution",
    fill = "Country"
  ) +
  theme(
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold")
  ) +
  coord_flip() 
