# By bkhanUVA - https://github.com/bkhanUVA

library(ggplot2)
library(dplyr)
library(tm)
library(wordcloud)
library(tidytext)
library(data.table)
library(tidyr)
library(stringr)
library(class)
library(rpart)
library(rpart.plot)
library(ipred)
library(ranger)
library(scales)

# Custom Vars for import
imdb_ratings_file_name <- "movies_ratings_sample.tsv"
imdb_metadata_file_name <- "movies_metadata_sample.tsv"
imdb_input_dir <- "~/Desktop/geu/Other_Stuff/imdbmovie/data"
user_ratings_input_dir <- "~/Desktop/geu/Other_Stuff/imdbmovie/data"
user_ratings_input_file_name <- "individual_user_ratings_sample.csv"
seed_id=420

# Utilities
import_tsv_util <- function(path, file_name, delim) {
  imported_data <- read.delim(
    file=paste(path, file_name, sep="/"),
    sep=delim, fileEncoding='UTF-8', 
    header=TRUE, fill=TRUE, stringsAsFactors = FALSE
  )
  return(imported_data)
}

transform_df_genres <- function(ratings_df, to_rows=TRUE) {
  # Take comma delimited GENRES column & create one row for each genre
  if (to_rows==TRUE) {
    ratings_long_df <- ratings_df %>% 
      mutate(GENRES = strsplit(as.character(GENRES), ",")) %>% 
      unnest(GENRES)
    return(ratings_long_df)
  } else {
    ratings_df$GENRES <- toupper(ratings_df$GENRES)
    distinct_genres_dirty <- distinct(ratings_df, GENRES) %>%
      pull(GENRES)
    distinct_genres <- paste(unlist(distinct_genres_dirty), collapse=',')
    distinct_genres <- unlist(strsplit(distinct_genres, ',')) %>%
      unique()
    
    ratings_wide_df <- ratings_df
    for(genre in distinct_genres) {
      #ratings_df[genre] <- genre %in% ratings_df[GENRES]
      ratings_wide_df <- extract(
        ratings_wide_df, GENRES, genre, 
        regex=paste0('(',genre,')'), remove=FALSE
      )
    }
    
    genres_subset <- select(ratings_wide_df, distinct_genres)
    genres_subset[!is.na(genres_subset)] <- 1
    genres_subset[is.na(genres_subset)] <- 0
    ratings_wide_df <- select(ratings_wide_df, -distinct_genres)
    ratings_wide_df <- cbind(ratings_wide_df, genres_subset)
    
    return(ratings_wide_df)
  }
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))] 
}

# Import Logic
read_imdb_input <- function() {
  # Read movie ratings and metadata downloaded from IMDB by pull_imdb_data.sh
  movie_ratings_df <- import_tsv_util(
    imdb_input_dir, imdb_ratings_file_name, "\t"
  )
  movie_metadata_df <- import_tsv_util(
    imdb_input_dir, imdb_metadata_file_name, "\t"
  )
  merged_imdb_movie_metadata_df <- inner_join(
    movie_ratings_df, movie_metadata_df, by = "tconst"
  )
  names(merged_imdb_movie_metadata_df) <- toupper(
    names(merged_imdb_movie_metadata_df)
  )
  return(merged_imdb_movie_metadata_df)
}

read_user_ratings_input <- function() {
  # Read personal ratings user downloaded from their IMDB profile
  user_movie_ratings_df <- read.csv(
    paste(user_ratings_input_dir, user_ratings_input_file_name, sep='/'),
    stringsAsFactors = FALSE
  )
  names(user_movie_ratings_df) <- toupper(names(user_movie_ratings_df))
  return(user_movie_ratings_df)
}

# Interesting statistics
calculate_basic_statistics <- function(
  merged_imdb_movie_metadata_df, user_movie_ratings_df
) {
  stats_list = list(
    mode = Mode(user_movie_ratings_df$YOUR.RATING),
    median = median(user_movie_ratings_df$YOUR.RATING),
    user_mean = round(mean(user_movie_ratings_df$YOUR.RATING), 2),
    imdb_mean = round(mean(merged_imdb_movie_metadata_df$AVERAGERATING), 2),
    standard_dev = sd(user_movie_ratings_df$YOUR.RATING)
  )
  stats_list['user_rating_vs_avg_rating'] = (
    stats_list$imdb_mean - stats_list$user_mean
  )
  return(stats_list)
}

#Plots 
#Making density and histogram plot of overall ratings
generate_ratings_histogram_density_plots <- function(user_movie_ratings_df, stats_list) {
  hist_plot <- ggplot(user_movie_ratings_df, aes(x = YOUR.RATING)) +
    geom_histogram(bins = 10, fill = "yellow", col = "black") +
    theme_bw() + xlab("Rating") + ylab("Count") +
    ggtitle("Your IMDb Rating Distribution") +
    scale_x_continuous(breaks = c(1:10))

  dens_plot <- ggplot(user_movie_ratings_df, aes(x = YOUR.RATING)) +
    geom_density(col = "black", fill = "yellow", alpha = .8) + theme_bw() +
    xlab("Rating") + ylab("Density") + ggtitle("Your IMDb Rating Distribution") +
    geom_vline(xintercept = (mean(stats_list$user_mean)), linetype = 2) +
    scale_x_continuous(breaks = c(1:10))
  print("Displaying Histogram and Density Plots")
  print(hist_plot)
  print(dens_plot)
  return(list(hist=hist_plot, dens=dens_plot))
}

# Plot the user's average rating by Genre against the average imdb user's
plot_genre_mean_ratings <- function(user_ratings_long_df, stats_list) {
#Create means ratings for each genre
  
  myMeans <- user_ratings_long_df %>% group_by(GENRES) %>% 
      summarize(MyAvgRating = mean(YOUR.RATING))
  
  OverallMeans <- user_ratings_long_df %>% group_by(GENRES) %>%
      summarize(OverallAvgRating = mean(IMDB.RATING))

  IMDB_Mean_v_User <- stats_list$user_rating_vs_avg_rating %>% paste()

  #Plot my mean rating against average mean rating
  genre_ratings_plot <- ggplot(myMeans, aes(x = GENRES, y = MyAvgRating)) +
    geom_bar(position = "dodge", stat = "summary", fun.y = "mean", col = "black", 
             fill = "yellow", alpha = .9) +
    geom_point(data = OverallMeans, aes(y = OverallAvgRating, x = GENRES), 
               size = 4, shape = 4) +
    theme_bw() + xlab("Genres") + ylab("Your Average Rating (X = IMDB avg)") +
    scale_y_continuous(breaks = c(1:10)) +
    ggtitle(paste("Your ratings are", paste(IMDB_Mean_v_User), "points higher than the avg user's")) +
    expand_limits(y = 10) +
    theme(axis.text.x = element_text(hjust = 1)) +
    coord_flip()
  
  print(genre_ratings_plot)
  return(genre_ratings_plot)
}

#My Ratings by year
plot_ratings_by_year <- function(user_movie_ratings_df) {
  YearCount <- user_movie_ratings_df %>% group_by(YEAR) %>% count(YEAR) %>% arrange(desc(n))
  YearCountSig <- filter(YearCount, n >= 5)
  YearsSig <- filter(user_movie_ratings_df, YEAR %in% YearCountSig$Year)
  
  ratings_by_yr_plot <- ggplot(YearsSig, aes(x = YEAR, y = YOUR.RATING)) +
    geom_jitter(alpha = .8, size = 3, shape = 21, fill = "yellow", width = .1) + 
    theme_bw() + 
    scale_y_continuous(breaks = c(1:10)) +
    geom_smooth(method = "lm", se = FALSE, col = "black") +
    ylab("My ratings") + ggtitle("Ratings don't really change on movie year")
  
  print(ratings_by_yr_plot)
  return(ratings_by_yr_plot)
}

run_all_plots <- function(user_movie_ratings_df, user_ratings_long_df, stats_list, year_plot=FALSE) {
  hist_plot <- generate_ratings_histogram_density_plots(user_movie_ratings_df, stats_list)
  genre_plot <- plot_genre_mean_ratings(user_ratings_long_df, stats_list)
  
  if (year_plot==TRUE) {
    ratings_yr_plot <- plot_ratings_by_year(user_movie_ratings_df)
  } else {
    ratings_yr_plot <- 'User disabled ratings year plot'
  }

  return(list(hist_plt = hist_plot, genre_plt = genre_plot, ratings_yr_plt = ratings_yr_plot))
}

#Subset movies I gave negative and positive reviews
MyRatingsPositive <- filter(cleaned_Movies_Long, Your.Rating >= 6)
MyRatingsNegative <- filter(cleaned_Movies_Long, Your.Rating <= 5)

#Large trans
imdbLarge <- filter(imdbLarge, ratingCount >= 1000)
imdbLarge <- arrange(imdbLarge, desc(ratingCount))




#Inner Join with IDs
JoinedAllRatings <- inner_join(myMovies1, imdbLarge, by = c("Const" = "tid"))
JoinedAllRatings$MyMovie <- as.character(JoinedAllRatings$MyMovie)
JoinedAllRatings$Title.Type <- as.factor(JoinedAllRatings$Title.Type)
str(JoinedAllRatings)
JoinedAllRatingsML <- select(JoinedAllRatings, Title, Your.Rating, IMDb.Rating, Year, c(Action:Western))
str(JoinedAllRatingsML)



#My ratings vs overall - turn this into a correlation
lm(Your.Rating ~ IMDb.Rating, data = myMovies1) %>% summary()
mean(myMovies1$Your.Rating) - mean(myMovies1$IMDb.Rating)

ggplot(myMovies1, aes(x = IMDb.Rating, y = Your.Rating)) +
  geom_jitter(alpha = .8, size = 3, shape = 21, fill = "yellow", width = .1) +
  geom_smooth(method = "lm", col = "black") +
  theme_bw() +
  xlim(1, 11) +
  ylim(1, 11) +
  scale_y_continuous(breaks = 1:10) +
  scale_x_continuous(breaks = 1:10) + 
  ggtitle("My avg rating increases as imdb avg rating increases")




#Making Machine learning alg -- binarizating my ratings
BinaryRatings <- mutate(JoinedAllRatingsML, 
                 LikedMovie = ifelse(JoinedAllRatings$Your.Rating > 5, 1, 0))
BinaryRatings$LikedMovie <- as.numeric(BinaryRatings$LikedMovie)
BinaryRatings %>% filter(Action == 1) %>% count(LikedMovie)
Binary_Ratings_Label <- BinaryRatings$LikedMovie
GenresOnlyTrain <- select(BinaryRatings, c(Action:Western))
GenresOnlyTest <- select(imdbLarge, c(Action:Western))


#knn without considering the imdb rating
set.seed(seed_id)

resultsknn <- knn(train = GenresOnlyTrain, test = GenresOnlyTest, 
                  cl = Binary_Ratings_Label, k = 10, prob = "TRUE")
knnprob <- attr(resultsknn, "prob")
head(knnprob)
imdbLargeKNN <- mutate(imdbLarge, predicted = resultsknn, prob = knnprob)
imdbLargeKNN$predicted <- factor(imdbLargeKNN$predicted, levels = c(0,1), labels = c("dislike", "like"))
knnsuggestions <- select(imdbLargeKNN, title, imdbRating, predicted, prob)
knnsuggestions <- arrange(knnsuggestions, desc(prob))
head(subset(knnsuggestions, predicted == "dislike"), 10)
head(knnsuggestions, 11)
str(subset(knnsuggestions, prob == 1))
count(knnsuggestions, predicted)

####including ratings -- normalizing and setting up####
#normalized = (x-min(x))/(max(x)-min(x))
#Binary_Ratings_rat <- BinaryRatings
#Binary_Ratings_rat$IMDb.Rating <- (BinaryRatings$IMDb.Rating - min(BinaryRatings$IMDb.Rating))/(max(BinaryRatings$IMDb.Rating) - min(BinaryRatings$IMDb.Rating))
#str(Binary_Ratings_rat)
#GenresOnlyTrain_rat <- select(Binary_Ratings_rat, IMDb.Rating, c(Action:Western))
#imdbLarge_rat <- mutate(imdbLarge, IMDb.Rating = 
#                          (imdbLarge$imdbRating - min(imdbLarge$imdbRating))/
#                          (max(imdbLarge$imdbRating) - min(imdbLarge$imdbRating)))
#str(Binary_Ratings_rat)
#str(imdbLarge_rat)
#GenresOnlyTest_rat <- select(imdbLarge_rat, IMDb.Rating, c(Action:Western))
#head(GenresOnlyTest_rat)

####Adding rescaled ratings to the knn model####
#resultsknn_rat <- knn(train = GenresOnlyTrain_rat, test = GenresOnlyTest_rat, 
 #                     cl = Binary_Ratings_Label, k = 10, prob = "TRUE")
#knnprob_rat <- attr(resultsknn_rat, "prob")
#head(knnprob_rat)
#imdbLargeKNN_rat <- mutate(imdbLarge_rat, predicted = resultsknn_rat, prob = knnprob_rat)
#imdbLargeKNN_rat$predicted <- factor(imdbLargeKNN_rat$predicted, 
 #                                    levels = c(0,1), labels = c("dislike", "like"))
#knnsuggestions_rat <- select(imdbLargeKNN_rat, title, IMDb.Rating, predicted, prob)
#knnsuggestions_rat <- arrange(knnsuggestions_rat, desc(prob))
#head(subset(knnsuggestions_rat, predicted == "dislike"), 10)
#head(knnsuggestions_rat, 11)
#str(subset(knnsuggestions_rat, prob == 1))
#count(knnsuggestions_rat, predicted)

#rescale using 0 to 1 for imdbratings need to use ifelse for >=7 then knn
Binary_Ratings_rat2 <- BinaryRatings
Binary_Ratings_rat2$IMDb.Rating <- ifelse(BinaryRatings$IMDb.Rating >=7, 1, 0)
GenresOnlyTrain_rat2 <- select(Binary_Ratings_rat2, IMDb.Rating, c(Action:Western))
imdbLarge_rat2 <- mutate(imdbLarge, IMDb.Rating = ifelse(
  imdbLarge$imdbRating >=7, 1, 0))
str(imdbLarge_rat)
GenresOnlyTest_rat2 <- select(imdbLarge_rat2, IMDb.Rating, c(Action:Western))
head(GenresOnlyTest_rat2)

resultsknn_rat2 <- knn(train = GenresOnlyTrain_rat2, test = GenresOnlyTest_rat2, 
                      cl = Binary_Ratings_Label, k = 11, prob = "TRUE")
knnprob_rat2 <- attr(resultsknn_rat2, "prob")
head(knnprob_rat2)
imdbLargeKNN_rat2 <- mutate(imdbLarge_rat2, 
                            predicted = resultsknn_rat2, prob = knnprob_rat2)
imdbLargeKNN_rat2$predicted <- factor(imdbLargeKNN_rat2$predicted, 
                                     levels = c(0,1), labels = c("dislike", "like"))
knnsuggestions_rat2 <- select(imdbLargeKNN_rat2, tid, title, IMDb.Rating, predicted, prob)
knnsuggestions_rat2 <- arrange(knnsuggestions_rat2, desc(prob))
knnsuggestions_rat2 <- mutate(knnsuggestions_rat2, IMDb.Rating = imdbLarge$imdbRating)
knnsuggestions_rat2 <- anti_join(knnsuggestions_rat2, myMovies1, by = c("tid" = "Const"))
head(subset(knnsuggestions_rat2, predicted == "dislike"), 10)
head(knnsuggestions_rat2, 10)
str(subset(knnsuggestions_rat2, prob == 1))
count(knnsuggestions_rat2, predicted)

#Decision tree
tree_train <- select(BinaryRatings, IMDb.Rating, c(Action:LikedMovie))
tree_train$LikedMovie <- factor(tree_train$LikedMovie, 
                                      levels = c(0,1), labels = c("dislike", "like"))

tree_model <- bagging(LikedMovie ~ ., data = tree_train, coob = TRUE, minsplit = 5, maxdepth = 15)

imdbLarge_eval <- imdbLarge %>% mutate(IMDb.Rating = imdbRating)
imdbLarge_eval <- imdbLarge_eval %>% select(IMDb.Rating, c(Action:Western))

tree_pred <- predict(tree_model, imdbLarge_eval, type = "prob")
#tree_prob <- attr(tree_pred, "prob")
tree_pred <- as.data.frame(tree_pred)
tree_suggestions <- mutate(imdbLarge_rat2, predicted = tree_pred$like)
str(tree_suggestions)
tree_final <- arrange(tree_suggestions, -predicted)
tree_final <- select(tree_final, title, imdbRating, predicted)
unique(tree_final$predicted)
filter(tree_final, predicted == 1) %>% nrow()
head(tree_final, 10)
#rpart.plot(tree_model)

#logistic Regression
logistic_train <- select(BinaryRatings, IMDb.Rating, c(Action:LikedMovie))
logistic_train$LikedMovie <- factor(logistic_train$LikedMovie, 
                                levels = c(0,1), labels = c("dislike", "like"))
logistic_model <- glm(LikedMovie ~ ., data = logistic_train, family = "binomial")
logistic_pred <- predict(logistic_model, imdbLarge_eval, type = "response")
logistic_suggestions <- mutate(imdbLarge_rat2, predicted = logistic_pred)
logistic_final <- arrange(logistic_suggestions, -predicted)
logistic_final$predicted <- ifelse(logistic_final$predicted > .99, 1, logistic_final$predicted)
subset(logistic_final, predicted == 1) %>% nrow()
head(logistic_final, 10)

#decision forest?
forest_model <- ranger(LikedMovie ~ ., tree_train, num.trees = 2500, 
                       respect.unordered.factors = "order", 
                       seed = 420)

forest_pred <- predict(forest_model, imdbLarge_eval, type ="response")
forest_suggestions <- mutate(imdbLarge_rat2, predicted = forest_pred$predictions)
filter(forest_suggestions, predicted == "like")

#clean imdblarge
imdbLargeML <- select(JoinedAllRatingsML, Title, c(Action:Western))



main <- function() {
  merged_imdb_movie_metadata_df <- read_imdb_input()
  user_movie_ratings_df <- read_user_ratings_input()
  stats_list <- calculate_basic_statistics(
    merged_imdb_movie_metadata_df, user_movie_ratings_df
  )
  user_ratings_long_df <- transform_df_genres(user_movie_ratings_df)
  all_imdb_ratings_wide_df <- transform_df_genres(
    merged_imdb_movie_metadata_df, to_rows=FALSE
  )
  all_plots <- run_all_plots(
    user_movie_ratings_df, user_ratings_long_df, stats_list
  )
  return(
    list(merged_imdb_movie_metadata_df, user_movie_ratings_df, user_ratings_long_df, all_imdb_ratings_wide_df)
  )
}

x = main()

# Notes
# Chart graphics need to scale based on user's # of ratings
# Measure correlation between user rating and avg