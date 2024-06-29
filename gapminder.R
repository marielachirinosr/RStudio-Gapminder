# Install and load necessary packages
install.packages("gapminder")
library(gapminder)
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data
data <- gapminder

# Explore the dataset
summary(data)

head(data)

str(data)

sum(is.na(data))

# Filter data for the year 2007
data_2007 <- filter(data, year == 2007)
selected_data <- select(data_2007, country, continent, gdpPercap, lifeExp, pop)

# Plot GDP per capita vs. life expectancy
ggplot(selected_data, aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  labs(title = "GDP per Capita vs Life Expectancy (2007)",
       x = "GDP per Capita (log scale)",
       y = "Life Expectancy",
       size = "Population",
       color = "Continent") +
  theme_minimal()

# Calculate mean life expectancy by continent
mean_lifeExp <- selected_data %>%
  group_by(continent) %>%
  summarise(mean_lifeExp = mean(lifeExp))

print(mean_lifeExp)

# Plot life expectancy trends by continent
ggplot(data, aes(x = year, y = lifeExp, color = continent)) +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Life Expectancy Trends by Continent",
       x = "Year",
       y = "Life Expectancy") +
  theme_minimal()

# Linear regression model
model <- lm(lifeExp ~ gdpPercap + pop, data = data_2007)
summary(model)

ggplot(selected_data, aes(x = gdpPercap)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  scale_x_log10() +
  labs(title = "Distribution of GDP per Capita (2007)",
       x = "GDP per Capita (log scale)",
       y = "Frequency") +
  theme_minimal()

