# Load the necessary packages
library(tidyverse) # for data manipulation and visualization
library(caret) # for machine learning
library(gridExtra) # for arranging plots
library(cluster) # for clustering
library(arules) # for association rule mining

# Load the data
zomato_data <- read.csv("zomato_data.csv")

# Data Cleaning
zomato_data_clean <- zomato_data %>%
# Remove duplicate records
distinct() %>%
# Remove unnecessary columns
select(-c("URL", "Address", "Phone", "Menu_Item", "Reviews_list")) %>%
# Convert some columns to factors
mutate_if(is.character, as.factor) %>%
# Remove missing values
drop_na()

# Exploratory Data Analysis (EDA)
# Summary statistics
summary(zomato_data_clean)



# Question 1: What are the most popular cuisines in different regions?
zomato_cuisine <- zomato_data %>%
  group_by(city, cuisine) %>%
  summarize(count = n()) %>%
  slice_max(count, n = 5) %>%
  ungroup()

print(zomato_cuisine)

# Question 2: Which restaurants have the highest ratings and reviews?
zomato_top <- zomato_data %>%
  group_by(name, address) %>%
  summarize(mean_rating = mean(rating),
            mean_reviews = mean(votes)) %>%
  arrange(desc(mean_rating), desc(mean_reviews)) %>%
  head(10)

print(zomato_top)

# Question 3: What factors influence customer ratings and reviews?
zomato_corr <- zomato_data %>%
  select(rating, votes, cost, online_order, book_table) %>%
  mutate(online_order = ifelse(online_order == "Yes", 1, 0),
         book_table = ifelse(book_table == "Yes", 1, 0)) %>%
  cor()

kable(zomato_corr)

# Question 4: Can we predict a restaurant rating based on factors such as location, cuisine, price range, and online delivery options?
zomato_model <- zomato_data %>%
  select(rating, city, cuisine, price_range, online_order) %>%
  mutate(city = as.factor(city),
         cuisine = as.factor(cuisine),
         price_range = as.factor(price_range),
         online_order = ifelse(online_order == "Yes", 1, 0))

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(zomato_model$rating, p = 0.7, list = FALSE)
train_data <- zomato_model[train_index,]
test_data <- zomato_model[-train_index,]

# Fit a linear regression model to predict ratings
model <- lm(rating ~ ., data = train_data)
summary(model)

# Predict the ratings for the test data and calculate the RMSE
predicted_ratings <- predict(model, newdata = test_data)
rmse <- RMSE(predicted_ratings, test_data$rating)
print(paste("RMSE: ", rmse))


# Distribution of ratings
ggplot(zomato_data_clean, aes(x = Rating)) +
  geom_histogram(fill = "blue", binwidth = 0.5) +
  labs(title = "Distribution of Ratings")



# Top 10 locations by restaurant count
top_locations <- zomato_data_clean %>%
  group_by(Location) %>%
  summarise(restaurants = n()) %>%
  arrange(desc(restaurants)) %>%
  head(10)

ggplot(top_locations, aes(x = reorder(Location, restaurants), y = restaurants)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Location") +
  ylab("Number of Restaurants") +
  labs(title = "Top 10 Locations by Restaurant Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#Clustering
# Select the variables to be used in clustering
zomato_subset <- select(zomato_data, rating, cost, latitude, longitude)

# Standardize the variables
zomato_scaled <- scale(zomato_subset)

sil_width <- c(NA)
for (i in 2:10) {
  kmeans_fit <- kmeans(zomato_scaled, centers = i, nstart = 25)
  sil_width[i] <- silhouette(kmeans_fit$cluster, dist(zomato_scaled))$avg.width
}

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = " Width",
     type = "b")

# Based on the silhouette method, choose 5 clusters
zomato_clusters <- kmeans(zomato_scaled, 5)

# Visualize the clusters
ggplot(zomato_data, aes(x = longitude, y = latitude, color = factor(zomato_clusters$cluster))) +
  geom_point() +
  theme_minimal()

# Add cluster assignments to the original dataset
zomato_data$cluster <- zomato_clusters$cluster

# Analyze the characteristics of each cluster
zomato_clusters_summary <- zomato_data %>%
  group_by(cluster) %>%
  summarize(
    mean_rating = mean(rating),
    mean_cost = mean(cost),
    mean_latitude = mean(latitude),
    mean_longitude = mean(longitude)
  )

print(zomato_clusters_summary)