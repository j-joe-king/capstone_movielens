################################
# HarvardX Data Science Capstone Course
# Movielens Data Analysis R Script
# Author: Jonathan King
# Date: 20 June 2020
#################################

options(digits = 6)

# SECTION 1 : Prerequisites

# Ensure required R packages are installed
# tidyverse, caret, data.table, lubridate, ggplot

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

# Function declarations
#
# my_set_seed function tests for R version to ensure 'sample.kind = "Rounding"'
# parameter is set if required for version 3.6 onwards
#
# e.g. my_set_seed(200) does 'set.seed(200) for R versions prior to 3.6 and
# set.seed(200, sample.kind = "Rounding" for R versions 3.6 onwards

my_set_seed <- function(seed) {
  if(as.numeric(R.version$major)< 3 | as.numeric(R.version$minor < 6)) {
     set.seed(seed)
  }
  else {
    set.seed(seed, sample.kind = "Rounding")
  }
}  

# RMSE function: this is the function used to calculate the root mean squares errors
# for all models 

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# SECTION 2 : DOWNLOAD AND CLEAN DATA

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
data_summary <- movielens %>% 
  summarize(dataset = "Full Movielens 10M", 
            n_ratings = n(),
            n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

# Now do some manipulation of the movielens dataset to split the year of release
# from the title and to reformat the timestamp as date-time class

movielens <- movielens %>% mutate(released=substr(title,nchar(title)-4,nchar(title)-1),
                      title=substr(title,1,nchar(title)-7),
                      timestamp=as_datetime(timestamp))

# For possible later use, define the full list of movie genres contained within the dataset.
# Summarise the number of users and movies in the dataset

movie_genres <- unique(unlist(str_split(movielens$genres,"\\|")))

# SECTION 3 : Split Data into edx (used for developing the model) and validation
# datsets for calculating the final RMSE (and at no point for training the model)
# Validation set will be 10% of MovieLens data. Seed is set to 1 as specified in
# course requirements.

my_set_seed(1)

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]


# This section makes sure all movies and users in the validation dataset are also contained
# in the edx dataset.
#
# The semi-joins remove rows from the validation dataset without a movie or user match in the 
# edx dataset.
#
# The anti-join adds the removed rows back into the edx dataset

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Summarise the data in edx and validation
data_summary <- data_summary %>% 
  rbind(edx %>% summarize(dataset = "Training/Testing (90% of Movielens 10M)", 
                          n_ratings = n(),
                          n_users = n_distinct(userId), 
                          n_movies = n_distinct(movieId)))  %>%
  rbind(validation %>% summarize(dataset = "Validation (10% of Movielens 10M)", 
                                 n_ratings = n(),
                                 n_users = n_distinct(userId), 
                                 n_movies = n_distinct(movieId)))

save(data_summary, movie_genres, file = "./rdas/data_summary.rda")

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# SECTION 4 : Data Visualisation
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
ggsave("./figs/plot_avg_movie_rating.png", width = 4, height = 4)

# save data 
save(edx_top_10_movies_by_avg_rating,
     edx_bottom_10_movies_by_avg_rating,
     edx_top_5_movies_by_count,
     cor_avg_movie_to_count,
     file = "./rdas/movie_data_analysis.rda")

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
ggsave("./figs/plot_avg_user_rating.png", width = 4, height = 4)

save(edx_top_10_users_by_avg_rating,
     edx_bottom_10_users_by_avg_rating,
     edx_top_5_users_by_count,
     cor_avg_user_to_count,
     file = "./rdas/user_data_analysis.rda")

# Is there a time effect on movie ratings?
# Let's see how the average rating of the top 5 movies by 
# number of ratings given has varied over time

plot_top_5_movie_ratings_by_date <- edx %>% filter(movieId %in% edx_top_5_movies_by_count$movieId) %>% 
  mutate(short_title=substr(title,1,20)) %>%
  ggplot(aes(timestamp,rating)) + 
  geom_smooth() + 
  facet_grid(short_title~., scales="free") +
  theme(strip.text.y = element_text(size = 7)) +
  ggtitle("Average Rating for Top 5 Movies Over Time") +
  xlab("Time") + ylab("Average Rating")

ggsave("./figs/plot_top_5_movie_ratings_by_date.png", width = 4, height = 4)

# Is there a time effect on movie ratings?
# Let's see how the average rating of the top 5 movies by 
# number of ratings given has varied over time

plot_top_5_user_ratings_by_date <- edx %>% filter(userId %in% edx_top_5_users_by_count$userId) %>% 
  ggplot(aes(timestamp,rating)) + 
  geom_smooth() + 
  facet_grid(userId~., scales="free_y") +
  ggtitle("Average Rating for Top 5 Users Over Time") +
  xlab("Time") + ylab("Average Rating")

ggsave("./figs/plot_top_5_user_ratings_by_date.png", width = 4, height = 4)

# SECTION 5 : Create Training and Test Datasets

# Now create training and test sets from edx dataset
# set seed to 202 using my_set_seed function

my_set_seed(202)

# use the same method as in creation of edx and validation datasets to split
# edx into edx_train (about 90%) and edx_test datasets (about 10%). 

test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index]
temp <- edx[test_index]

edx_test <- temp %>%
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

removed <- anti_join(temp, edx_test)
edx_train <- rbind(edx_train, removed)

edx_data_summary <- edx_train %>% 
  summarize(dataset = "edx: Training Set", 
            n_ratings = n(),
            n_users = n_distinct(userId),
            n_movies = n_distinct(movieId)) %>%
  rbind(edx_test %>% summarize(dataset = "edx: Test Set", 
                               n_ratings = n(), 
                               n_users = n_distinct(userId),
                               n_movies = n_distinct(movieId)))

save(edx_data_summary, movie_genres, file = "./rdas/edx_data_summary.rda")

# Clean up
rm(temp, test_index, removed)


# SECTION 6 : Building the Recommendation System

# NAIVE AVERAGE MODEL
#
# The first model is the simplest: just predict everyone will rate the movie with the average
# mu_hat is our estimate of the average

mu_hat <- mean(edx_train$rating)

# Calculate Root Mean Squared Error and store in rmse_results
rmse_results <- data_frame(id = 1, 
                           method = "Naive Average Model",
                           rmse = RMSE(edx_test$rating, mu_hat))

# ADD PER-MOVIE BIAS MODEL
#
# Ideally, we would fit a linear model as follows:
#    fit <- lm(rating ~ as.factor(movieId), data = edx_train)
# using this on such a large dataset will be too time-consuming, so we approximate the movie effect with 
#    b_i = mean(rating-mu_hat) 
#
# For later use in regularisation of the data we save 
#    n_i = the number of ratings for the movie, 
# and for time effects, 
#    first_rated_i = the Monday on or before the movie's first rating

movie_avgs <- edx_train %>% group_by(movieId) %>% 
  summarize(n_i = n(),
            b_i = mean(rating - mu_hat,),
            first_rated_i = floor_date(min(timestamp), "weeks", week_start=1))

# Histogram of the distribution of b_i 
plot_b_i_distribution <- movie_avgs %>% qplot(b_i, geom ="histogram", bins = 30, data = ., color = I("black"))
ggsave("./figs/plot_b_i_distribution.png", width = 3, height = 3)

# Store the b_i, n_i and first_rated_i from movie_avgs against each rating in training and test datasets

edx_train <- edx_train %>% left_join(movie_avgs, by = "movieId")
edx_test <- edx_test %>% left_join(movie_avgs, by = "movieId")

# Predicted ratings on edx_test
predicted_ratings <- mu_hat + edx_test$b_i

# Calculate Root Mean Squared Error and store in rmse_results
rmse_results <- bind_rows(rmse_results,
                          data_frame(id=2,
                                     method="Movie Bias Model",
                                     rmse = RMSE(predicted_ratings, edx_test$rating) ))

# ADD PER-USER BIAS MODEL
# 
# We now approximate the user bias as the residual of the residual bias after the movie bias
# has been taken into account:
#    b_u = mean(rating - mu_hat - b_i). 
# For later use in regularisation we save 
#    n_u = the number of ratings for the user, 
# and for time effects, 
#    first_rated_u = the Monday on or before the user's first rating


user_avgs <- edx_train %>% 
  group_by(userId) %>%
  summarize(n_u = n(),
            b_u = mean(rating - mu_hat - b_i),
            first_rated_u = floor_date(min(timestamp), "weeks", week_start=1))

# Histogram of the distribution of b_i 
plot_b_u_distribution <- user_avgs %>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("black"))
ggsave("./figs/plot_b_u_distribution.png", width = 3, height = 3)

edx_train <- edx_train %>% left_join(user_avgs, by = "userId")
edx_test <- edx_test %>% left_join(user_avgs, by = "userId")

# Predicted ratings on edx_test
predicted_ratings <- mu_hat + edx_test$b_i + edx_test$b_u

# Calculate Root Mean Squared Error and store in rmse_results
rmse_results <- bind_rows(rmse_results,
                          data_frame(id=3,
                                     method="Movie and User Biases Model",  
                                     rmse = RMSE(predicted_ratings, edx_test$rating) ))

# NOW REGULARISE MOVIE AND USER BIAS MODEL
#
# Now we use penalized least squares to tune the movie and user effects
# First find the lambda that provides the best penalised least squares estimate

lambdas <- seq(0.25,10,0.25)
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- 
    edx_test %>% 
    mutate(pred = 
             mu_hat + 
             b_i*n_i/(n_i+l) + 
             b_u*n_u/(n_u+l)) %>%
    .$pred
  return(RMSE(predicted_ratings, edx_test$rating))
})

# plot of rmse v lambda
plot_lambda_v_rmse <- data_frame(l = lambdas, rmse = rmses) %>% 
  ggplot(aes(lambdas, rmse))  + geom_point() +
  xlab("\u03bb")

ggsave("./figs/plot_lambda_v_rmse.png", width = 3, height = 3)

# chooose lambda to minimise RMSE and store the best RMSE from the rmses vector
lambda <- lambdas[which.min(rmses)]
description <- "Regularised Movie and User Biases Model"
rmse_results <- bind_rows(rmse_results, 
                          data_frame(id=4,
                                     method=description, 
                                     rmse = min(rmses),
                                     lambda = lambda))

# NOW ADD (REGULARISED) TIME-DEPENDENT MOVIE BIAS 

# Firstly, let's model a timed movie effect by allocating ratings to time 'bins'. Intuitively, since
# movies are released on a weekly basis, it makes sense to use bins based on weeks. Let's optimise
# the number of weeks in each time 'bin' to minimise the RMSE. We'll choose the best bin size from 
# 1 to 52 weeks. Longer than this seems counter-intuitive.

# NB At this stage we use the lambda calculated already to regularise the timed movie effect
#    d_i(Bin t) = mean(rating - mu_hat - b_i - b_u) for all ratings for movie i in Bin t

# Optimise the bin size: this takes a minute or two...

binsize <- seq(1,52,3)
rmses <- sapply(binsize, function(size){
  # calculate d_i = the residual time-dependent movie effect
  movies_by_bin <- edx_train %>% 
    mutate(movie_bin = 1+round(time_length(interval(first_rated_i, timestamp), "week")/size)) %>%
    group_by(movieId, movie_bin) %>%
    summarise(n_di = n(),
              d_i = mean(rating - b_i - b_u - mu_hat))
  
  # calculate predicted ratings incorporating d_i, with all effects regularised with lambda
  
  df <- edx_test %>% 
    mutate(movie_bin = 1+round(time_length(interval(first_rated_i, timestamp), "week")/size)) %>%
    left_join(movies_by_bin, by = c("movieId","movie_bin")) 
  
  # set d_i to 0 where no corresponding row in movies_by_bin
  df$d_i[is.na(df$d_i)] <- 0
  df$n_di[is.na(df$n_di)] <- 0
  
  predicted_ratings <- df %>% 
    mutate(pred = 
             mu_hat + 
             b_i * n_i/(n_i + lambda) +
             b_u * n_u/(n_u + lambda) +
             d_i * n_di/(n_di + lambda)) %>% 
    pull(pred)
  # calculate RMSE
  return(RMSE(predicted_ratings, edx_test$rating))
})

# choose optimal bin size and save corresponding RMSE from rmses vector
optimal_binsize_i <- binsize[which.min(rmses)]
description <- "Regularised Movie and User Biases with Added Time Dependent Movie Bias Model"
rmse_results <- bind_rows(rmse_results, data_frame(id = 5,
                                                   method=description, 
                                                   rmse = min(rmses),
                                                   lambda = lambda,
                                                   binsize_i = optimal_binsize_i))

plot_binsize_i_v_rmse <- data_frame(binsize, rmse=rmses) %>% 
  ggplot(aes(binsize,rmses)) + geom_point() + xlab("binsize") + ylab("RMSE")

ggsave("./figs/plot_binsize_i_v_rmse.png", width = 3, height = 3) 

# add d_i, n_di for optimal bin size to the edx_train and edx_test datasets
edx_train <- edx_train %>% 
  mutate(movie_bin = 1+round(time_length(interval(first_rated_i, timestamp), "week")/optimal_binsize_i)) 
  
movies_over_time <- edx_train %>% 
  group_by(movieId, movie_bin) %>%
  summarise(n_di = n(), 
            d_i = mean(rating - b_i - b_u - mu_hat))

edx_train <- edx_train %>% 
  left_join(movies_over_time, by = c("movieId","movie_bin"))

edx_test <- edx_test %>% 
  mutate(movie_bin = 1+round(time_length(interval(first_rated_i, timestamp), "week")/optimal_binsize_i)) %>%
  left_join(movies_over_time, by = c("movieId","movie_bin"))

edx_test$d_i[is.na(edx_test$d_i)] <- 0 
edx_test$n_di[is.na(edx_test$n_di)] <- 0 

# NOW ADD (REGULARISED) TIME-DEFPENDENT USER BIAS 

# Let's model a timed user effect by allocating ratings to time 'bins'. From earlier analysis,
# it's likely user ratings are more volatile than movie ratings, so it make sense to choose a smaller
# bin size.

# NB At this stage we use the lambda calculated already to regularise the timed movie effect
#    d_u(Bin t) = mean(rating - mu_hat - b_i - b_u - d_i) for all ratings for user u in Bin t

# Optimise the bin size: this takes a minute or two...
binsize <- seq(1,4)
rmses <- sapply(binsize, function(size){
  # calculate d_u = the residual time-dependent movie effect
  users_by_bin <- edx_train %>% 
    mutate(user_bin = 1+round(time_length(interval(first_rated_u, timestamp), "week")/size)) %>%
    group_by(userId, user_bin) %>%
    summarise(n_du = n(),
              d_u = mean(rating - b_i - d_i - b_u - mu_hat))
  
  # calculate predicted ratings incorporating d_i, with all effects regularised with lambda
  
  df <- edx_test %>% 
    mutate(user_bin = 1+round(time_length(interval(first_rated_u, timestamp), "week")/size)) %>%
    left_join(users_by_bin, by = c("userId","user_bin")) 
  
  # set d_i to 0 where no corresponding row in movies_by_bin
  df$d_u[is.na(df$d_u)] <- 0
  df$n_du[is.na(df$n_du)] <- 0 
  
  predicted_ratings <- df %>% 
    mutate(pred = 
             mu_hat + 
             b_i * n_i/(n_i + lambda) +
             b_u * n_u/(n_u + lambda) +
             d_i * n_di/(n_di + lambda) +
             d_u * n_du/(n_du + lambda)) %>% 
    pull(pred)
  # calculate RMSE
  return(RMSE(predicted_ratings, edx_test$rating))
})

# choose optimal bin size and save corresponding RMSE from rmses vector

optimal_binsize_u <- binsize[which.min(rmses)]
description <- "Regularised Static and Time-Dependent Movie and User Biases Model"
rmse_results <- bind_rows(rmse_results, data_frame(id = 6,
                                                   method=description, 
                                                   rmse = min(rmses),
                                                   lambda = lambda,
                                                   binsize_i = optimal_binsize_i,
                                                   binsize_u = optimal_binsize_u))

plot_binsize_u_v_rmse <- data_frame(binsize, rmse=rmses) %>% 
  ggplot(aes(binsize,rmses)) + geom_point() + 
  xlab("binsize") + ylab("RMSE")

ggsave("./figs/plot_binsize_u_v_rmse.png", width = 3, height = 3) 

# add d_u, n_du for optimal bin size to the edx_train and edx_test datasets
edx_train <- edx_train %>% 
  mutate(user_bin = 1+round(time_length(interval(first_rated_u, timestamp), "week")/optimal_binsize_u))
  
users_over_time <- edx_train %>% 
  group_by(userId, user_bin) %>%
  summarise(n_du = n(), 
            d_u = mean(rating - b_i - d_i - b_u - mu_hat))

edx_train <- edx_train %>% 
  left_join(users_over_time, by = c("userId","user_bin"))

edx_test <- edx_test %>% 
  mutate(user_bin = 1+round(time_length(interval(first_rated_u, timestamp), "week")/optimal_binsize_u)) %>%
  left_join(users_over_time, by = c("userId","user_bin"))

edx_test$d_u[is.na(edx_test$d_u)] <- 0 
edx_test$n_du[is.na(edx_test$n_du)] <- 0 

# Originally we derived lambda in our penalized least squares on just the b_i and b_u effects.
# We now optimise lambda based on all the modelled effects, i.e. including as well d_i and d_u
# First find the lambda that provides the best penalised least squares estimate

lambdas <- seq(0.25,15,0.25)
rmses <- sapply(lambdas, function(l){
  #genre_residual_penalised <- rowMeans(b_k * n_k/(n_k+l) * genre_matrix_test)
  predicted_ratings <- #genre_residual_penalised +
    edx_test %>% 
    mutate(pred = 
             mu_hat + 
             b_i*n_i/(n_i+l) + 
             b_u*n_u/(n_u+l) +
             d_i*n_di/(n_di+l) +
             d_u*n_du/(n_du+l)) %>%
    .$pred
  return(RMSE(predicted_ratings, edx_test$rating))
})

# plot of rmse v lambda
plot_final_lambda_v_rmse <- data_frame(l = lambdas, rmse = rmses) %>% 
  ggplot(aes(lambdas, rmse))  + geom_point() +
  xlab("\u03bb") + ylab("RMSE") + ggtitle("RMSE v \u03bb: Final Model")

ggsave("./figs/plot_final_lambda_v_rmse.png", width = 3, height = 3)

final_lambda <- lambdas[which.min(rmses)]
description <- "Final Regularised Static and Time-Dependent Movie and User Biases Model"
rmse_results <- bind_rows(rmse_results, data_frame(id = 7,
                                                   method=description, 
                                                   rmse = min(rmses),
                                                   lambda = final_lambda,
                                                   binsize_i = optimal_binsize_i,
                                                   binsize_u= optimal_binsize_u))

# SECTION 7 : CALCULATE RMSE ON THE VALIDATION DATASET

# Allocate estimated b_i, b_u, d_i, d_u and associated counts n_i, n_u, n_di, n_du to the validation
# dataset by matching up with the training dataset

validation <- validation %>% left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(movie_bin = 1+round(time_length(interval(first_rated_i, timestamp), "week")/optimal_binsize_i),
         user_bin = 1+round(time_length(interval(first_rated_u, timestamp), "week")/optimal_binsize_u)) %>%
  left_join(movies_over_time, by = c("movieId","movie_bin")) %>%
  left_join(users_over_time, by = c("userId","user_bin"))

validation$d_i[is.na(validation$d_i)] <- 0 
validation$n_di[is.na(validation$n_di)] <- 0 
validation$d_u[is.na(validation$d_u)] <- 0 
validation$n_du[is.na(validation$n_du)] <- 0 


# Calculated predicted ratings for the validation dataset

predicted_ratings <-
  validation %>% 
  mutate(pred = 
           mu_hat + 
           b_i*n_i/(n_i+final_lambda) + 
           b_u*n_u/(n_u+final_lambda) +
           d_i*n_di/(n_di+final_lambda)+
           d_u*n_du/(n_du+final_lambda)) %>%
  .$pred

# FINALLY! Calculate the RMSE for the validation set for the final model

validation_rmse <- RMSE(predicted_ratings, validation$rating)

# save results 
save(mu_hat, 
     rmse_results, 
     lambda,
     final_lambda,
     optimal_binsize_i,
     optimal_binsize_u,
     validation_rmse,
     file = "./rdas/results.rda")


