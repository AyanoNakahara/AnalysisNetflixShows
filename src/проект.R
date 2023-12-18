library(tidyverse)
library(tidytext)
library(tm)
library(ggplot2)
library(viridis)
library(wordcloud2)
library(wordcloud)
library(maps)
library(dplyr)
library(scales)

# Import dataset and glimpse
netflix <- read_csv("netflix_titles.csv")
glimpse(netflix)

#clean
netflix_clean <- drop_na(netflix)

# renaming the column listed_in in to Genre
# then only showing the first Genre in Genre column
netflix_clean <- netflix_clean %>%
  rename(Genre = listed_in) %>%
  mutate(Genre = strsplit(Genre, ',')[sapply(strsplit(Genre, ','), function(x) length(x) > 0)])


# Get summary
summary(netflix_clean)


#Get the type of NetFlix Shows

ggplot(netflix_clean, aes(x = type)) + 
  geom_bar(aes(fill = type))


# Count of shows by year
count_in_year <- netflix_clean %>%
  group_by(release_year) %>%
  summarise(count = n())

# Increase the size of the plot
options(repr.plot.width = 10, repr.plot.height = 6)

# Create the bar plot with breaks for every 5 years
ggplot(count_in_year, aes(x = release_year, y = count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Year", y = "Count", title = "Count of Shows by Year") +
  scale_x_continuous(breaks = seq(min(count_in_year$release_year), max(count_in_year$release_year), by = 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  theme_minimal()




# Top 10 countries content on Netflix
top_countries <- netflix_clean %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:10)

# Print the structure of 'top_countries'
str(top_countries)

# Print the column names of 'netflix_clean'
colnames(netflix_clean)

# Plotting
ggplot(data = filter(netflix_clean, country %in% top_countries$country), 
       aes(x = country, fill = type)) +
  geom_bar(position = "dodge") +
  geom_text(data = top_countries, 
            aes(x = country, y = count, label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3, inherit.aes = FALSE) +
  scale_fill_manual(values = c("pink", "lightgreen")) +
  labs(title = "Top 10 Countries Content On Netflix",
       x = "Country",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Top 10 ratings given to the show
top_ratings <- head(table(netflix_clean$rating), 10)

netflix_clean %>%
  count(rating) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(aes(x = "", y = n, fill = rating)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(color = "black", size = 16, face = "bold")) +
  ggtitle("Distribution Of Rating") +
  geom_text(aes(label = scales::percent(n/sum(n)),
                x = 1.3,  
                y = n),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "black", # Add a white color to the text for better visibility
            show.legend = FALSE,  # Hide the legend
            angle = 45) +  # Adjust the angle for better separation
  guides(fill = guide_legend(title = NULL))




# Mean duration of movies by years
# Set options to suppress warnings
options(warn=-1)

# Filter data for movies released between 2000 and 2022
movie_past_years <- subset(netflix_clean, type == 'Movie' & release_year %in% 2000:2022)

# Convert duration to numeric
movie_past_years$duration <- as.numeric(gsub("[^0-9.]", "", movie_past_years$duration))

# Create two plots side by side
par(mfrow=c(1,2), mar=c(5, 5, 2, 2), oma=c(0, 0, 2, 0), pty="m", cex.main=1.5)

# Plot mean duration of movies over the years
plot(duration ~ release_year, 
     data = aggregate(duration ~ release_year, data = movie_past_years, FUN = mean),
     type = "o", 
     main = "Mean duration of Movies by years",
     xlab = "Release Year",
     ylab = "Mean duration (min)",
     xaxt = "n",
     col = "purple",
     pch = 16)

axis(1, at = seq(2000, 2022, by = 1), labels = TRUE, las = 2)





# Extract the first genre from the list
netflix_clean$Genre <- sapply(netflix_clean$Genre, function(genres) genres[[1]])

# Convert 'Genre' to a factor
netflix_clean$Genre <- as.factor(netflix_clean$Genre)

# Create a data frame for prediction
prediction_data <- expand.grid(
  release_year = unique(netflix_clean$release_year),
  Genre = unique(netflix_clean$Genre)
)

# Train a linear regression model to predict the count of entries (number of films)
model <- lm(n ~ release_year + Genre, data = netflix_clean %>%
              group_by(Genre, release_year) %>%
              summarise(n = n()))

# Predict the count of entries using the linear regression model
prediction_data$predicted_count <- predict(model, newdata = prediction_data)

# Display the predictions
print(prediction_data)


# Plot the predicted counts
ggplot(prediction_data, aes(x = release_year, y = predicted_count, color = Genre, group = Genre)) +
  geom_line() +
  geom_point() +
  labs(title = "Predicted Number of Films by Genre Over the Years",
       x = "Release Year",
       y = "Predicted Count") +
  scale_x_continuous(breaks = seq(min(prediction_data$release_year), max(prediction_data$release_year), by = 2)) +
  theme_minimal()



# Create a data frame for prediction in 2024
prediction_2024 <- expand.grid(
  release_year = 2024,
  Genre = unique(netflix_clean$Genre)
)

# Predict the count of entries for 2024 using the linear regression model
prediction_2024$predicted_count <- predict(model, newdata = prediction_2024)

# Display the predictions for 2024
print(prediction_2024)




# Add 2024 predictions to the original prediction_data
combined_predictions <- rbind(prediction_data, prediction_2024)

# Round the predicted counts
combined_predictions$rounded_predicted_count <- round(combined_predictions$predicted_count)

# Specify the break interval (change this to 2 or 3 as needed)
break_interval <- 2

y_break_interval <- 10

# Plot the combined predictions with modified breaks
plot <- ggplot(combined_predictions, aes(x = release_year, y = rounded_predicted_count, color = Genre)) +
  geom_line() +
  geom_point() +
  labs(title = "Predicted Number of Films by Genre Over the Years",
       x = "Release Year",
       y = "Predicted Number of Films",
       color = "Genre") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(combined_predictions$release_year), max(combined_predictions$release_year), by = break_interval)) +
  scale_y_continuous(breaks = seq(min(combined_predictions$rounded_predicted_count), max(combined_predictions$rounded_predicted_count), by = y_break_interval))

# Display the modified plot
print(plot)

# model summary
summary(model)
