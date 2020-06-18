################################
# Create edx set, validation set
# JJK 10 June 2020 (a) Added explanatory note about creation of validation and edx datasets
# JJK 12 June 2020 (b) Processing of edx into test and training sets added
# JJK 12 June 2020 (d) my_set_seed function added to allow setting of seed regardless of R version
#################################

# Prerequisites: ensure required R packages are installed
# Install R packages tidyverse, caret, data.table if not already loaded

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

# Create subdirectories for .rda files and figures
if (!file.exists("./rdas")){
  dir.create("./rdas")
}

if (!file.exists("./figs")){
  dir.create("./figs")
}

# Create subdirectories for .rda files and figures
if (!file.exists("./rdas")){
  dir.create("./rdas")
}

if (!file.exists("./figs")){
  dir.create("./figs")
}

# Function declarations

# my_set_seed function tests for R version to ensure 'sample.kind = "Rounding"'
# parameter is set if required for version 3.6 onwards
# e.g. my_set_seed(200) does 'set.seed(200) for R versions prior to 3.6 and
# set.seed(200, sample.kind = "Rounding"
my_set_seed <- function(seed) {
  if(as.numeric(R.version$major)< 3 | as.numeric(R.version$minor < 6)) {
     set.seed(seed)
  }
  else {
    set.seed(seed, sample.kind = "Rounding")
  }
}  

# RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Summarise the data in movielens
movielens_summary <- movielens %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

# Now do some manipulation of the movielens dataset to split the year of release
# from the title and to reformat the timestamp as date-time class

movielens <- movielens %>% mutate(released=substr(title,nchar(title)-4,nchar(title)-1),
                      title=substr(title,1,nchar(title)-7),
                      timestamp=as_datetime(timestamp))

# For later use, define the full list of movie genres contained within the dataset.
# Summarise the number of users and movies in the dataset

movie_genres <- unique(unlist(str_split(movielens$genres,"\\|")))
movielens_summary <- movielens_summary %>% cbind(n_genres=length(movie_genres))

save(movielens_summary, movie_genres, file = "./rdas/movielens.rda")

# Validation set will be 10% of MovieLens data

my_set_seed(1)

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]


# (a) JJK 10 June 2020 
# This section makes sure all movies and users in the validation dataset are also contained
# in the edx set.
#
# The semi-joins remove rows from the validation set without a movie or user match in the 
# edx dataset.
# The anti-join adds the removed rows back into the edx

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Data Visualisation
# Let's look at the edx dataset and draw some inferences

# Is there a movie effect?
# Group the data by movieId, calculating average rating and standard
# deviation, and setting the count of ratings

edx_by_movie <- edx %>% 
  group_by(movieId,title,genres) %>% 
  summarise(avg_rating=mean(rating), 
            sd=sd(rating), count=n()) %>% 
  arrange(desc(avg_rating)) %>% ungroup

# Before regularisation of any data let's look at the top
# and bottom 10 movies by average ratings
edx_top_10_movies_by_avg_rating <- head(edx_by_movie,10)
edx_bottom_10_movies_by_avg_rating <- tail(edx_by_movie,10)

# And now the top 5 movies by number of ratings
edx_top_5_movies_by_count <-
  edx_by_movie %>% arrange(desc(count)) %>% slice(1:5)

# Plot the average rating by number of ratings
# Highlight our top and bottom 10 movies by average rating and
# our top 5 movies by count
plot_avg_movie_rating <- edx_by_movie %>% ggplot(aes(count,avg_rating,color=count)) +
  scale_colour_gradient(name = "Number of Ratings", low="rosybrown",high = "Blue",
                        breaks = c(1000, 10000, 20000,30000)) + 
  geom_point(alpha=0.6, shape=1, color="rosybrown") +
  geom_point(data=edx_top_5_movies_by_count, shape = 17, color="Blue") +
  geom_point(data=rbind(edx_top_10_movies_by_avg_rating,
                        edx_bottom_10_movies_by_avg_rating), 
             shape = 17, color="Red") +
  ggtitle("Average Movie Ratings (edx Data Set)") +
  xlab("Number of Ratings") + ylab("Average Rating")

# Correlation avg_rating~count
cor_avg_movie_to_count = cor(edx_by_movie$avg_rating, edx_by_movie$count)

# save plot
ggsave("./figs/plot_avg_movie_rating.png")

# save data 
save(edx_top_10_movies_by_avg_rating,
     edx_bottom_10_movies_by_avg_rating,
     edx_top_5_movies_by_count,
     cor_avg_movie_to_count,
     file = "./rdas/movielens.rda")

# Is there a user effect?
# Group the data by userId

edx_by_user <- edx %>%
  group_by(userId) %>%
  summarise(avg_rating=mean(rating), sd=sd(rating), count=n()) %>% 
  arrange(desc(avg_rating)) %>%
  ungroup

# Before regularisation of any data let's look at the top
# and bottom 10 users by average ratings
edx_top_10_users_by_avg_rating <- head(edx_by_user,10)
edx_bottom_10_users_by_avg_rating <- tail(edx_by_user,10)

# And now the top 5 movies by number of ratings
edx_top_5_users_by_count <-
  edx_by_user %>% arrange(desc(count)) %>% slice(1:5)

# Plot the average rating by number of ratings
# Highlight our top and bottom 10 users by average rating and
# our top 5 users by count
plot_avg_user_rating <- edx_by_user %>% ggplot(aes(count,avg_rating,color=count)) +
  scale_colour_gradient(name = "Number of Ratings", low="rosybrown",high = "Blue",
                        breaks = c(1000, 10000, 20000,30000)) + 
  geom_point(alpha=0.6, shape=1, color="rosybrown") +
  geom_point(data=edx_top_5_users_by_count, shape = 17, color="Blue") +
  geom_point(data=rbind(edx_top_10_users_by_avg_rating,
                        edx_bottom_10_users_by_avg_rating), 
             shape = 17, color="Red") +
  ggtitle("Average User Ratings (edx Data Set)") +
  xlab("Number of Ratings") + ylab("Average Rating")

# Correlation avg_rating~count
cor_avg_user_to_count = cor(edx_by_user$avg_rating, edx_by_user$count)

# save plot
ggsave("./figs/plot_avg_user_rating.png")

# calculate correlation between number of ratings and 
save(edx_top_10_users_by_avg_rating,
     edx_bottom_10_users_by_avg_rating,
     edx_top_5_users_by_count,
     cor_avg_user_to_count,
     file = "./rdas/movielens.rda")

# Is there a time effect on movie ratings?
# Let's see how the average rating of the top 5 movies by 
# number of ratings given has varied over time

plot_top_5_movie_ratings_by_date <- edx %>% filter(movieId %in% edx_top_5_movies_by_count$movieId) %>% 
  mutate(short_title=substr(title,1,20)) %>%
  ggplot(aes(timestamp,rating)) + 
  geom_smooth() + facet_grid(short_title~., scales="free") +
  theme(strip.text.y = element_text(size = 7)) +
  ggplot(aes(timestamp,rating)) + 
  geom_smooth() + facet_grid(title~., scales="free") +
  ggtitle("Average Rating for Top 5 Movies Over Time") +
  xlab("Time") + ylab("Average Rating")

ggsave("./figs/plot_top_5_movie_ratings_by_date.png")

# 
# (b) JJK 12 June 2020 Now create training and test sets from edx dataset
# set seed to 202 using my_set_seed function
my_set_seed(202)
# use the same method as in creation of edx and validation datasets to split
# edx into training (about 90%) and test datasets (about 10%). 

test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index]
temp <- edx[test_index]

edx_test <- temp %>%
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

removed <- anti_join(temp, edx_test)
edx_train <- rbind(edx_train, removed)

# Clean up
rm(temp, test_index, removed)


### Building the Recommendation System
# simplest model is just assuming the overall average rating
mu_hat <- mean(edx_train$rating)

naive_rmse <- RMSE(edx_test$rating, mu_hat)
naive_rmse

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

# fit <- lm(rating ~ as.factor(movieId), data = edx_train): using this on this large
# dataset will be too time-consuming, so we approximate the movie effect with
# mean(rating-mu_hat)

movie_avgs <- edx_train %>% group_by(movieId) %>% 
  summarize(n_i = n(),
            b_i = mean(rating - mu_hat))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

edx_train <- edx_train %>% left_join(movie_avgs, by = "movieId")
edx_test <- edx_test %>% left_join(movie_avgs, by = "movieId")

predicted_ratings <- mu_hat + edx_test$b_i

model_1_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

# fit <- lm(rating ~ as.factor(movieId), data = edx_train): using this on this large
# dataset will be too time-consuming, so we approximate the movie effect with
# b_i = mean(rating-mu_hat)

edx_train %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

# lm(rating ~ as.factor(movieId) + as.factor(userId)): as before this would be time
# consuming, so we're approximating the additional user effect with 
# b_u = mean(rating-mu_hat-b_i)

user_avgs <- edx_train %>% 
  group_by(userId) %>%
  summarize(n_u = n(),
            b_u = mean(rating - mu_hat - b_i))

edx_train <- edx_train %>% left_join(user_avgs, by = "userId")
edx_test <- edx_test %>% left_join(user_avgs, by = "userId")

predicted_ratings <- mu_hat + edx_test$b_i + edx_test$b_u

model_2_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))

# Is there a time effect?
# Calculate a week number based on earliest rating in dataset
# Week 1 starts on the Monday of that week

earliest_monday <- floor_date(min(edx_train$timestamp), "weeks", week_start=1)

edx_train <- edx_train %>% mutate(weekno = 1 + round(time_length(interval(earliest_monday, timestamp), "week")))
edx_test <- edx_test %>% mutate(weekno = 1 + round(time_length(interval(earliest_monday, timestamp), "week")))

weekly_avgs <- edx_train %>% 
  group_by(weekno) %>%
  summarize(n_d = n(), 
            d_ui = mean(rating - b_i - b_u - mu_hat))

plot_weekly_avgs <- weekly_avgs %>% ggplot(aes(weekno,d_ui)) +
  geom_smooth()

edx_train <- edx_train %>% left_join(weekly_avgs, by = "weekno")
edx_test <- edx_test %>% left_join(weekly_avgs, by = "weekno")

predicted_ratings <- mu_hat + edx_test$b_i + edx_test$b_u + edx_test$d_ui

model_3_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Time Effects Model",  
                                     RMSE = model_3_rmse ))


# Now we use penalized least squares to tune the movie, user and time effects
# First find the lambda that provides the best penalised least squares estimate

lambdas <- seq(0.25,10,0.25)
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- 
    edx_test %>% 
    mutate(pred = 
             mu_hat + 
             b_i*n_i/(n_i+l) + 
             b_u*n_u/(n_u+l) +
             d_ui*n_d/(n_d+l)) %>%
    .$pred
  return(RMSE(predicted_ratings, edx_test$rating))
})

# plot of rmse v lambda
plot_lambda_v_rmse <- data_frame(l = lambdas, rmse = rmses) %>% 
  ggplot(aes(lambdas, rmse))  + geom_point() +
  xlab("\u03bb")
lambda <- lambdas[which.min(rmses)]
rmse_results <- bind_rows(rmse_results, data_frame(method="Regularised Movie + User + Time Effects Model",
                                                   RMSE = rmses[which.min(rmses)]))

# save results 
save(mu_hat, rmse_results,lambda,"./rdas/movielens.rda")

removed <- anti_join(temp, edx_test)
edx_train <- rbind(edx_train, removed)

# Clean up
rm(temp, test_index, removed)
