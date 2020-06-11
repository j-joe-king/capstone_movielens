# Quiz: MovieLens Dataset
# JJK 10 June 2020 

#q1
nrow(edx)
ncol(edx)

#q2
sum(edx$rating==0)
sum(edx$rating==3)

#q3
length(unique(edx$movieId))
#q4
length(unique(edx$userId))
#q5
sum(edx$genres %like% "Drama")
sum(edx$genres %like% "Comedy")
sum(edx$genres %like% "Thriller")
sum(edx$genres %like% "Romance")
#q6
edx %>% group_by(movieId) %>%
  summarise(noof_ratings=n(),title=title[1]) %>% 
  arrange(desc(noof_ratings)) %>% 
  slice(1:10)

#q7
edx %>% group_by(rating) %>% summarise(noof_ratings=n()) %>% arrange(desc(noof_ratings))
                                       