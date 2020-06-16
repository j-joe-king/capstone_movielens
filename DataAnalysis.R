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

