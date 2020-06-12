################################
# Create edx set, validation set
# JJK 10 June 2020 (a) Added explanatory note about creation of Validation and edx datasets
# JJK 12 June 2020 (b) Added conditional processing so that data is downloaded once
# JJK 12 June 2020 (c) Processing of edx into test and training sets added
# JJK 12 June 2020 (d) my_set_seed function added to allow setting of seed regardless of R version
################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# (d) JJK 12 June 2020 my_set_seed function tests for R version to ensure
# sample.kind = "Rounding" parameter is set if required for version 3.6 onwards
my_set_seed <- function(seed) {
  if(as.numeric(R.version$major)< 3 | as.numeric(R.version$minor < 6)) {
     set.seed(seed)
  }
  else {
    set.seed(seed, sample.kind = "Rounding")
  }
}  

# Note: this process could take a couple of minutes

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

# Validation set will be 10% of MovieLens data

my_set_seed(1)

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]


# (a) JJK 10 June 2020 
# This section makes sure all ratings for a given movie are either in the 
# validation or the edx dataset, not split across them.
# The semi-joins remove the split user-movie ratings from temp before assigning to validation
# The anti-join adds the removed split user-movie ratings back into edx

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# (c) JJK 12 June 2020 Now create training and test sets from edx dataset
# set seed to 2 using my_set_seed function
my_set_seed(2)
# use the same method as in creation of edx and validation datasets to split
# edx into training and test datasets. This ensures that individual movies and users
# are not split across training and test datasets

test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.5, list = FALSE)
train_set <- edx[-test_index]
temp <- edx[test_index]

test_set <- temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

# Analysis of rows removed from temp to train_set
size_removed <- nrow(removed)
no_users_removed <- length(unique(removed$userId))
no_movies_removed <- length(unique(removed$movieId))
# Analysis of test and train set user and movie distribution
size_train_set <- nrow(train_set)
no_users_train_set <-length(unique(train_set$userId))
no_movies_train_set <- length(unique(train_set$movieId))

size_test_set <- nrow(test_set)
no_users_test_set <-length(unique(test_set$userId))
no_movies_test_set <- length(unique(test_set$movieId))

# Clean up
rm(temp, test_index)




