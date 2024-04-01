# libraries required
library(dplyr)
library(tidyr)
library(car)
library(ggplot2)
library(ggpubr)
library(e1071)
library(reshape2)
library(psych)
library(caret)
library(stargazer)
library(cvTools)
library(dplyr)
library(boot)
library(vcd)
library(tm)
library(SnowballC)
require(lmtest)
require(plm)


imdb = read.csv('midterm-project/MGSC-661-Project/final/IMDB_data_Fall_2023.csv')
test = read.csv('midterm-project/MGSC-661-Project/final/test_data_IMDB_Fall_2023.csv')

add_genres = function(df) {
  df$documentary <- ifelse(grepl("Documentary", df$genres), 1, 0)
  df$biography <- ifelse(grepl("Biography", df$genres), 1, 0)
  df$fantasy <- ifelse(grepl("Fantasy", df$genres), 1, 0)
  df$comedy <- ifelse(grepl("Comedy", df$genres), 1, 0)
  df$mystery <- ifelse(grepl("Mystery", df$genres), 1, 0)
  df$family <- ifelse(grepl("Family", df$genres), 1, 0)

  df$action <- ifelse(grepl("Action", df$genres), 1, 0)+ifelse(grepl("action", df$plot_keywords), 1, 0)
  df$action  = ifelse(df$action >= 1,1,0)

  df$adventure <- ifelse(grepl("Adventure", df$genres), 1, 0)+ifelse(grepl("adventure", df$plot_keywords), 1, 0)
  df$adventure  = ifelse(df$adventure >= 1,1,0)

  df$scifi <- ifelse(grepl("Sci-Fi", df$genres), 1, 0)+ifelse(grepl("scifi", df$plot_keywords), 1, 0)
  df$scifi = ifelse(df$scifi >= 1,1,0)

  df$thriller <- ifelse(grepl("Thriller", df$genres), 1, 0)+ifelse(grepl("thriller", df$plot_keywords), 1, 0)
  df$thriller  = ifelse(df$thriller >= 1,1,0)

  df$musical <- ifelse(grepl("Music", df$genres), 1, 0)+ifelse(grepl("musical", df$plot_keywords), 1, 0)
  df$musical  = ifelse(df$musical >= 1,1,0)

  df$romance <- ifelse(grepl("Romance", df$genres), 1, 0)+ifelse(grepl("romance", df$plot_keywords), 1, 0)
  df$romance  = ifelse(df$romance >= 1,1,0)

  df$western <- ifelse(grepl("Western", df$genres), 1, 0)+ifelse(grepl("western", df$plot_keywords), 1, 0)
  df$western  = ifelse(df$western >= 1,1,0)

  df$sport <- ifelse(grepl("Sport", df$genres), 1, 0)+ifelse(grepl("sport", df$plot_keywords), 1, 0)
  df$sport  = ifelse(df$sport >= 1,1,0)

  df$horror <- ifelse(grepl("Horror", df$genres), 1, 0)+ifelse(grepl("horror", df$plot_keywords), 1, 0)
  df$horror  = ifelse(df$horror >= 1,1,0)

  df$drama <- ifelse(grepl("Drama", df$genres), 1, 0)+ifelse(grepl("drama", df$plot_keywords), 1, 0)
  df$drama  = ifelse(df$drama >= 1,1,0)

  df$war <- ifelse(grepl("War", df$genres), 1, 0)+ifelse(grepl("war", df$plot_keywords), 1, 0)
  df$war  = ifelse(df$war >= 1,1,0)

  df$animation <- ifelse(grepl("Animation", df$genres), 1, 0)+ifelse(grepl("animation", df$plot_keywords), 1, 0)
  df$animation  = ifelse(df$animation >= 1,1,0)

  df$crime <- ifelse(grepl("Crime", df$genres), 1, 0)+ifelse(grepl("crime", df$plot_keywords), 1, 0)
  df$crime  = ifelse(df$crime >= 1,1,0)

  df$documentary <- ifelse(grepl("Documentary", df$genres), 1, 0)+ifelse(grepl("documentary", df$plot_keywords), 1, 0)
  df$documentary  = ifelse(df$documentary >= 1,1,0)

  df$biography <- ifelse(grepl("Biography", df$genres), 1, 0)+ifelse(grepl("biography", df$plot_keywords), 1, 0)
  df$biography  = ifelse(df$biography >= 1,1,0)

  df$fantasy <- ifelse(grepl("Fantasy", df$genres), 1, 0)+ifelse(grepl("fantasy", df$plot_keywords), 1, 0)
  df$fantasy  = ifelse(df$fantasy >= 1,1,0)

  df$comedy <- ifelse(grepl("Comedy", df$genres), 1, 0)+ifelse(grepl("comedy", df$plot_keywords), 1, 0)
  df$comedy  = ifelse(df$comedy >= 1,1,0)

  df$mystery <- ifelse(grepl("Mystery", df$genres), 1, 0)+ifelse(grepl("mystery", df$plot_keywords), 1, 0)
  df$mystery  = ifelse(df$mystery >= 1,1,0)

  df$family <- ifelse(grepl("Family", df$genres), 1, 0)+ifelse(grepl("family", df$plot_keywords), 1, 0)
  df$family  = ifelse(df$family >= 1,1,0)

  return(df)
}

add_maturities = function(df) {
  maturity_categories <- unique(df$maturity_rating)

  # Loop through each category and perform linear regression
  for (category in maturity_categories) {
    # Create a dummified variable for the current category
    df[category] <- as.numeric(df$maturity_rating == category)

  }
  
  if ("TV-G" %in% colnames(df)){
  df['TV.G'] = df["TV-G"]}
  if ('TV-14' %in% colnames(df)){
  df['TV.14'] = df["TV-14"]}
  if ('PG-13' %in% colnames(df)){
  df['PG.13'] = df['PG-13']}
  if ('NC-17' %in% colnames(df)){
  df['NC.17'] = df['NC-17']}
  return(df)
}


imdb <- add_genres(imdb)
imdb <- add_maturities(imdb)


add_maturity_test = function(test){
maturity =  unique(test$maturity_rating)

for (r in maturity){
  test[r] =  as.numeric(test$maturity_rating == r)
}

test['TV.G'] = 0
test['TV.14'] = 0
test['PG.13'] = test['PG-13']

return(test)
}


test <- add_genres(test)
test <- add_maturity_test(test)


extract_stem_words <- function(text) {
  text <- tolower(text)
  words <- unlist(strsplit(text, "\\|"))
  stemmed_words <- wordStem(words, language = "en")
  return(stemmed_words)
}

stemmed_plot_keywords <- sapply(test$plot_keywords, extract_stem_words)


for (keyword in stemmed_plot_keywords) {
  imdb[[keyword]] <- ifelse(grepl(keyword, imdb$plot_keywords), 1, 0)
}


add_logs = function(df) {
  df$log_nb_news_articles = log(df$nb_news_articles)
  df$log_movie_meter_IMDBpro = log(df$movie_meter_IMDBpro)
  df$log_movie_budget = log(df$movie_budget)
  df$log_duration = log(df$duration)
  df$log_actor1_star_meter = log(df$actor1_star_meter)

  df$log_nb_news_articles <- ifelse(is.infinite(df$log_nb_news_articles) & df$log_nb_news_articles < 0, 0, df$log_nb_news_articles)
  df$log_movie_meter_IMDBpro <- ifelse(is.infinite(df$log_movie_meter_IMDBpro) & df$log_movie_meter_IMDBpro < 0, 0, df$log_movie_meter_IMDBpro)

  if (is.String(df$movie_budget)){
    df$movie_budget = as.numeric(gsub(",", "", df$movie_budget))
  }

  df$log_movie_budget <- ifelse(is.infinite(df$log_movie_budget) & df$log_movie_budget < 0, 0, df$log_movie_budget)
  df$log_duration <- ifelse(is.infinite(df$log_duration) & df$log_duration < 0, 0, df$log_duration)
  
  df$log_actor1_star_meter <- ifelse(is.infinite(df$log_actor1_star_meter) & df$log_actor1_star_meter < 0, 0, df$log_actor1_star_meter)



  return(df)
}


imdb<- add_logs(imdb)
test<- add_logs(test)


add_lengths = function(test) {
  test$actor1_length = lapply(test$actor1,nchar)
  test$genres_length = lapply(test$genres,nchar)
  test$distributor_length = lapply(test$distributor,nchar)
  test$movie_title_length = lapply(test$movie_title,nchar)

  test$actor1_length = as.numeric(test$actor1_length)
  test$genres_length = as.numeric(test$genres_length)
  test$distributor_length = as.numeric(test$distributor_length)
  test$movie_title_length = as.numeric(test$movie_title_length)

  return(test)
}

imdb <- add_lengths(imdb)
test <- add_lengths(test)

add_others = function(test) {
  test$is_Miramax = ifelse(grepl('Miramax', test$production_company), 1,0)
  test$is_Nov = ifelse(grepl('Nov', test$release_month), 1,0)
  test$is_Dec = ifelse(grepl('Dec', test$release_month), 1,0)
  test$is_color = ifelse(test$colour_film=="Color",1,0)


  return(test)
}

imdb <- add_others(imdb)
test <- add_others(test)


lm_final = lm(imdb_score~
                log_movie_budget+
                log_duration+
                poly(log_nb_news_articles, 1, raw = TRUE)+
                poly(log_movie_meter_IMDBpro, 4)+
                is_color+
                poly(genres_length,2)+
                biography+
                documentary+
                is_Miramax+
                horror+
                drama+
                documentary+
                biography+
                animation+
                R+
                TV.14+
                is_color
              ,data = imdb)

summary(lm_final)

outlierTest(lm_final)
imdb_without_outliers = imdb[-c(1806,1581,191,395,1436,1255,989),]


glm_final = glm(imdb_score~log_movie_budget +
                  log_duration+
                  poly(log_nb_news_articles, 1, raw = TRUE)+
                  poly(log_movie_meter_IMDBpro, 4)+
                  is_color+
                  poly(genres_length,2)+
                  biography+
                  animation+
                  documentary+
                  is_Miramax+
                  horror+
                  drama+
                  documentary+
                  biography+
                  animation+
                  R+
                  TV.14+
                  is_color
                , data = imdb_without_outliers)

# our final predictions
pred = predict(glm_final, newdata = test)

cbind(test$movie_title,as.vector(pred)) 
summary(glm_final)

# Supplementary code for the report
get_cv_mse <- function(model_formula, data, K) {
  mse_distribution <- numeric(100)

  for (i in 1:100) {
    fit <- glm(model_formula, data = data)
    mse_distribution[i] <- cv.glm(data, fit, K = K)$delta[1]
  }

  print(mse_distribution)
  hist(mse_distribution)
}

get_cv_mse(glm_final,imdb_without_outliers,5)

# stargazer code
names(glm_final$coefficients) = c("Intercept","Log of Movie Budget", "Log of Duration", "Log of Number of News Articles",
                                  "Log of Movie Meter", "Log of Movie Meter^2", "Log of Movie Meter^3", "Log of Movie Meter^4",
                                  "Is Color", "Length of Genres", "Length of Genres^2", "Biography", "Animation",
                                  "Documentary", "Produced by Miramax", "Horror", "Drama", "Rated R", "Rated TV-14")

stargazer(glm_final, type = "text", align = TRUE,digits = 2, dep.var.labels = "IMDB Score")


# Log Transformation charts
log_transformation_charts <- function(imdb) {
  par(mfrow = c(2, 2))
  hist(imdb$actor1_star_meter, breaks = 10, main = "Distribution of Actor 1's Star Meter", xlab = "Star Meter")
  hist(log(imdb$actor1_star_meter), breaks = 10, main = "Distribution of Log of Actor 1 Star Meter", xlab = "Log of Star Meter")

  par(mfrow = c(2, 2))
  hist(imdb$actor2_star_meter, breaks = 10, main = "Distribution of Actor 2's Star Meter", xlab = "Star Meter")
  hist(log(imdb$actor2_star_meter), breaks = 10, main = "Distribution of Log of Actor 2 Star Meter", xlab = "Log of Star Meter")

  par(mfrow = c(2, 2))
  hist(imdb$actor3_star_meter, breaks = 10, main = "Distribution of Actor 3's Star Meter", xlab = "Star Meter")
  hist(log(imdb$actor3_star_meter), breaks = 10, main = "Distribution of Log of Actor 3's Star Meter", xlab = "Log of Star Meter")

  par(mfrow = c(2, 2))
  hist(imdb$nb_news_articles, breaks = 100, main = "Distribution of News Articles", xlab = "Count")
  hist(log(imdb$log_nb_news_articles), breaks = 100, main = "Distribution of Log of News Articles", xlab = "Log Count")

  par(mfrow = c(2, 2))
  hist(imdb$movie_meter_IMDBpro, breaks = 100, main = "Distribution of Movie Meter (IMDBpro)", xlab = "Meter")
  hist(log(imdb$log_movie_meter_IMDBpro), breaks = 100, main = "Distribution of Log of Movie Meter (IMDBpro)", xlab = "Log of Meter")

  par(mfrow = c(2, 2))
  hist(imdb$duration, breaks = 100, main = "Distribution of Duration", xlab = "Duration")
  hist(log(imdb$duration), breaks = 100, main = "Distribution of Log of Duration", xlab = "Log of Duration")

  par(mfrow = c(2, 2))
  hist(imdb$movie_budget, breaks = 100, main = "Distribution of Movie Budget", xlab = "Budget")
  hist(log(imdb$movie_budget), breaks = 100, main = "Distribution of Log of Movie Budget", xlab = "Log of Budget")
  #

}

log_transformation_charts(imdb)

cor_heatmap <- function(imdb) {
  numeric_df <- imdb[c("movie_budget", "release_day", "release_year", "duration", "nb_news_articles", "actor1_star_meter",
                       "actor2_star_meter", "actor3_star_meter", "nb_faces","movie_meter_IMDBpro","imdb_score")]
  # Rename variables in numeric_df
  names(numeric_df) <- c("Movie Budget", "Release Day", "Release Year", "Duration","Number of News Articles",
                         "Actor 1 Star Meter","Actor 2 Star Meter", "Actor 3 Star Meter", "Number of Faces",
                         "Movie Meter (IMDB Pro)", "IMDB Score")

  cor_matrix <- cor(numeric_df)
  # Melt the correlation matrix for ggplot
  cor_melted <- melt(cor_matrix)
  ggplot(data = cor_melted, aes(x = Var1, y = Var2)) +
    geom_tile(aes(fill = value)) +
    geom_text(aes(label = round(value, 2)), size = 3) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                         limit = c(min(cor_melted$value), max(cor_melted$value)), name = "Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    coord_fixed()
}

cor_heatmap(imdb)


