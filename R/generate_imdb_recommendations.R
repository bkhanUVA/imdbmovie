# By bkhanUVA - https://github.com/bkhanUVA

##########################
#     Environment        #
##########################
library(ggplot2)
library(dplyr)
library(tidyr)
library(class)

# User's Custom Vars
imdb_ratings_file_name <- "movies_ratings_sample.tsv"
imdb_metadata_file_name <- "movies_metadata_sample.tsv"
imdb_input_dir <- "~/Desktop/geu/Other_Stuff/imdbmovie/data"
user_ratings_input_dir <- "~/Desktop/geu/Other_Stuff/imdbmovie/data"
user_ratings_input_file_name <- "individual_user_ratings_sample.csv"
seed_id=420
genres_list <- toupper(c("Documentary", "Short", "Animation", "Comedy", 
                         "Romance","Sport", "Action", "News", "Drama", 
                         "Fantasy", "Horror", "Biography", "Music", "War",
                         "Crime", "Western", "Family", "Adventure", "History",
                         "Mystery", "Sci-Fi", "Thriller",  "Musical"))
final_cols <- toupper(c('CONST', 'binary_ratings_imdb', 'binary_ratings_user',
                        'PRIMARYTITLE', 'AVERAGERATING', 'IMDB.RATING'))


##########################
#       Utilities        #
##########################
import_tsv_util <- function(path, file_name, delim) {
  # IMDB API delivers tab separated files
  imported_data <- read.delim(
    file=paste(path, file_name, sep="/"),
    sep=delim, fileEncoding='UTF-8', 
    header=TRUE, fill=TRUE, stringsAsFactors = FALSE
  )
  return(imported_data)
}


transform_df_genres <- function(ratings_df, to_rows=TRUE) {
  # Take comma delimited GENRES column & create one row for each genre
  #  or if to_rows = FALSE then create a column for each genre (in this case
  #  genres associated w/ the movie record are signified w/ 1)
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


##########################
# Data Import & Cleaning #
##########################
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


##########################
#  Statistics for plots  #
##########################
calculate_basic_statistics <- function(
  merged_imdb_movie_metadata_df, user_movie_ratings_df
) {
  stats_list = list(
    user_movies_rated_cnt = length(user_movie_ratings_df),
    mode = Mode(user_movie_ratings_df$YOUR.RATING),
    median = median(user_movie_ratings_df$YOUR.RATING),
    user_mean = round(mean(user_movie_ratings_df$YOUR.RATING), 2),
    imdb_mean = round(mean(merged_imdb_movie_metadata_df$AVERAGERATING), 2),
    standard_dev = sd(user_movie_ratings_df$YOUR.RATING),
    correlation = cor.test(
      user_movie_ratings_df$YOUR.RATING,
      user_movie_ratings_df$IMDB.RATING,
      method='pearson'
    )
  )
  stats_list['user_rating_vs_avg_rating'] = (
    stats_list$imdb_mean - stats_list$user_mean
  )
  return(stats_list)
}


##########################
#         Plots          #
##########################
generate_ratings_histogram_density_plots <- function(user_movie_ratings_df,
                                                     stats_list) {
  # Making density and histogram plots of user's overall ratings
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


plot_genre_mean_ratings <- function(user_ratings_long_df, stats_list) {
# Plot the user's average rating by Genre against the average imdb user's
  # Create means ratings for each genre
  myMeans <- user_ratings_long_df %>% group_by(GENRES) %>% 
      summarize(MyAvgRating = mean(YOUR.RATING))
  OverallMeans <- user_ratings_long_df %>% group_by(GENRES) %>%
      summarize(OverallAvgRating = mean(IMDB.RATING))
  IMDB_Mean_v_User <- stats_list$user_rating_vs_avg_rating %>% paste()

  # Plot user's mean rating against average mean rating
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


plot_ratings_by_year <- function(user_movie_ratings_df) {
  # Plot how ratings vary by year
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
  # Run all plotting functions
  hist_plot <- generate_ratings_histogram_density_plots(user_movie_ratings_df, stats_list)
  genre_plot <- plot_genre_mean_ratings(user_ratings_long_df, stats_list)
  
  # Year plot is buggy on smaller datasets
  if (year_plot==TRUE) {
    ratings_yr_plot <- plot_ratings_by_year(user_movie_ratings_df)
  } else {
    ratings_yr_plot <- 'User disabled ratings year plot'
  }
  
  return(list(hist_plt = hist_plot, genre_plt = genre_plot, ratings_yr_plt = ratings_yr_plot))
}


##########################
#     Prep & run KNN     #
##########################
merge_and_clean_final_imdb_df <- function(all_imdb_ratings_wide_df,
                                          user_movie_ratings_df) {
  # Merge user ratings with all ratings so KNN can create predictions
  #  Remove movies with few views (< 1000 ratings)
  #  binarize ratings (>= 6 means you like the movie, <6 = dislike)
  movies_w_decent_sample_size_df <- filter(
    all_imdb_ratings_wide_df, NUMVOTES >= 1000
    )
  
  final_imdb_df <- full_join(
    user_movie_ratings_df, all_imdb_ratings_wide_df,  by=c('CONST'='TCONST')
  )
  
  final_imdb_df <- mutate(
    final_imdb_df, 
    BINARY_RATINGS_USER = ifelse(final_imdb_df$YOUR.RATING >= 6, 1, 0),
    BINARY_RATINGS_IMDB = ifelse(final_imdb_df$AVERAGERATING >= 6, 1, 0)
  ) %>% 
    select_if(names(.) %in% c(final_cols, genres_list))
  return(final_imdb_df)
}


run_knn_report_results <- function(final_imdb_df, user_movie_ratings_df,
                                   stats_list) {
  # I reworked this quickly so it just runs... results & statistics are likely bad
  
  # tmp / bad solution so i can get knn to run... need to fix this
  final_imdb_df <- filter(final_imdb_df, !is.na(BINARY_RATINGS_IMDB))
  
  binary_ratings_label <- final_imdb_df$BINARY_RATINGS_USER
  
  knn_train_df <- final_imdb_df %>% 
    select_if(names(.) %in% c('BINARY_RATINGS_IMDB', genres_list))
  
  knn_test_df <- final_imdb_df %>% select_if(names(.) %in% c('BINARY_RATINGS_IMDB', genres_list))
  
  # remove this later
  knn_test_df <- na.omit(knn_test_df)
  knn_train_df <- na.omit(knn_train_df)
  binary_ratings_label[is.na(binary_ratings_label)] <- 0

  # KNN uses 3% of user's ratings to determine # of nearest neighbors to match
  #  to deal with abnormal rating counts, min nearest is 3 and max is 25
  knn_results <- knn(
    train = knn_train_df,
    test = knn_test_df, 
    cl = binary_ratings_label,
    k = (
      min(max(ceiling(stats_list$user_movies_rated_cnt*.03), 3), 25)
    ),
    prob = "TRUE"
  )
  
  knn_probability <- attr(knn_results, "prob")
  
  imdb_final_knn_df <- mutate(
    final_imdb_df, PREDICTED = knn_results, PROB = knn_probability
  )
  imdb_final_knn_df$PREDICTED <- factor(
    imdb_final_knn_df$PREDICTED,
    levels = c(0,1),
    labels = c("dislike", "like")
  )
  
  print(imdb_final_knn_df)
  knn_suggestions_df <- select(
    imdb_final_knn_df, CONST, PRIMARYTITLE, AVERAGERATING, PREDICTED, PROB
  ) %>% 
    arrange(desc(PROB))
  final_knn_suggestions_df <- anti_join(knn_suggestions_df, user_movie_ratings_df, by = c("CONST" = "CONST"))
  
  print("Most Likely to dislike:")
  print(head(subset(final_knn_suggestions_df, PREDICTED == "dislike"), 10))
  print("Most likely to like:")
  top_suggestions_df <- head(final_knn_suggestions_df, 10)
  print(top_suggestions_df)
  print("Misc:")
  print(str(subset(final_knn_suggestions_df, PROB == 1)))
  print(count(final_knn_suggestions_df, PREDICTED))
  return(top_suggestions_df)
}


##########################
#     Main Execution     #
##########################
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
  final_ratings_df <- merge_and_clean_final_imdb_df(
    all_imdb_ratings_wide_df, user_movie_ratings_df
  )
 recommendations <- run_knn_report_results(
   final_ratings_df, select(user_movie_ratings_df, CONST), stats_list
  )
  return(
    list(
      merged_imdb_movie_metadata = merged_imdb_movie_metadata_df,
      user_movie_ratings = user_movie_ratings_df,
      stats = stats_list,
      user_ratings_long = user_ratings_long_df,
      all_imdb_ratings_wide = all_imdb_ratings_wide_df,
      final_ratings = final_ratings_df,
      final_recommendations = recommendations
    )
  )
}

# Run script
final_run_results <- main()

# Notes (see issues in github for more)
# i'm training on my final run dataset lol
# standard function input / output
# remove user movies df input to knn functin