## ----setup, include=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE
)


## ---------------------------------------------------------------------------------------
version


## ---------------------------------------------------------------------------------------
################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
 # https://grouplens.org/datasets/movielens/10m/
 # http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
 download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
 colnames(movies) <- c("movieId", "title", "genres")
 movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
 title = as.character(title), genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")


## ---------------------------------------------------------------------------------------
# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
 edx <- movielens[-test_index,]
 temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
      semi_join(edx, by = "movieId") %>%
      semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
 edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


## ---------------------------------------------------------------------------------------
str(edx)
str(validation)


## ---------------------------------------------------------------------------------------
# Modify the timestamp column

library(lubridate)
edx$timestamp <- as_datetime(edx$timestamp)
validation$timestamp <- as_datetime(validation$timestamp)

# Modify the title column

edx<- edx %>% separate(title, c("name", "year"), sep="\\s*\\((?=\\d+\\)$)|\\)$", convert=TRUE)
validation<- validation %>% separate(title, c("name", "year"), sep="\\s*\\((?=\\d+\\)$)|\\)$", convert=TRUE)


## ---------------------------------------------------------------------------------------
str(edx)


## ---------------------------------------------------------------------------------------
sum(is.na(c(edx,validation)))


## ---------------------------------------------------------------------------------------
edx %>% ggplot(aes(x=rating,y=..density..)) + geom_histogram(binwidth = 0.5, colour = "black") + scale_x_discrete(limits = c(seq(0,5,0.5))) + ggtitle("Distribution of Ratings")


## ---------------------------------------------------------------------------------------
edx %>% count(name)%>%arrange(desc(n))%>%head(6)
edx %>% count(name)%>%arrange(-desc(n))%>%head(6)


## ---------------------------------------------------------------------------------------
edx %>% count(movieId) %>% ggplot(aes(n))+ geom_density(fill = 'black',colour = 'black')+ ggtitle('Distribution of ratings per movie')+ labs(x = "Ratings per movie", y = "Density") + scale_x_log10() + geom_vline(aes(xintercept=mean(n)), colour="red", linetype="solid", size=1) + geom_vline(aes(xintercept=median(n)),color="blue", linetype="solid", size=1)+annotate("text", x = 1500, y = 0.12, label = "Mean", color="Red") + annotate("text", x = 50, y = 0.12, label = "Median", colour="Blue")


## ---------------------------------------------------------------------------------------
movie_total_counts <- edx %>% group_by(movieId) %>% summarise(total_ratings=n()) 
quantiles_1 <- quantile(movie_total_counts$total_ratings, probs=0.95)
movie_total_counts %>% ggplot(aes(total_ratings)) + stat_ecdf(geom = "step") + labs(title = "Empirical Cumulative Distribution Function of Ratings per Movie",x = "Number of Movies",y = "Cumulative Proportion of Movie Ratings") + geom_vline(aes(xintercept=quantiles_1, colour = "red",linetype="solid")) + annotate("text", x = 8000, y = 0.50, label = "95% of Ratings", colour="red") + theme(legend.position = "none")


## ---------------------------------------------------------------------------------------
edx %>% count(userId) %>% ggplot(aes(n))+ geom_density(fill = 'black', colour = 'black')+ ggtitle('Distribution of ratings per consumer')+ labs(x = "Ratings per consumer", y = "Density") + scale_x_log10() + geom_vline(aes(xintercept=mean(n)),colour="red", linetype="solid", size=1) + geom_vline(aes(xintercept=median(n)),colour="blue", linetype="solid", size=1) + annotate("text", x = 200, y = 0.22, label = "Mean", color="Red") + annotate("text", x = 35, y = 0.22, label = "Median", colour="Blue")


## ---------------------------------------------------------------------------------------
user_total_counts <- edx %>% group_by(userId) %>% summarise(total_ratings=n()) 
quantiles_2 <- quantile(user_total_counts$total_ratings, probs=0.95)
user_total_counts %>% ggplot(aes(total_ratings)) + stat_ecdf(geom = "step") + labs(title = "Empirical Cumulative Distribution Function of Ratings per Consumer", x = "Number of Consumers", y = "Cumulative Proportion of Movie Ratings") + geom_vline(aes(xintercept=quantiles_2, colour = "red",linetype="solid")) + annotate("text", x = 1250, y = 0.50, label = "95% of Ratings", colour="red") + theme(legend.position = "none")


## ---------------------------------------------------------------------------------------
RMSE <- function(actual_ratings, predicted_ratings){
     sqrt(mean((actual_ratings - predicted_ratings)^2, na.rm=TRUE))}


## ---------------------------------------------------------------------------------------
test_index <- createDataPartition(edx$rating, times = 1, p = 0.5, list = FALSE)
edx_test<- edx[test_index,]
edx_train<- edx[-test_index,]


## ---------------------------------------------------------------------------------------
mu_hat_train<- mean(edx_train$rating)
rmse_1 <- RMSE(edx_test$rating, mu_hat_train)
reg_1 <- data.frame(Approach = "Naive Baseline from Expected Value", RMSE = rmse_1)
reg_1 %>% knitr::kable()


## ---------------------------------------------------------------------------------------
actual_movie_ratings <- edx_train %>% group_by(movieId) %>% summarise(b_i = mean(rating - mu_hat_train))

predicted_movie_ratings<-  mu_hat_train + edx_test %>% left_join(actual_movie_ratings, by= "movieId", na.rm=TRUE) %>% .$b_i

predicted_movie_ratings<- as.numeric(predicted_movie_ratings)

rmse_2 <- RMSE(edx_test$rating, predicted_movie_ratings)

reg_2 <- data.frame(Approach = "Regression Model with Movie Effect",RMSE = rmse_2)

reg_2 %>% knitr::kable()


## ---------------------------------------------------------------------------------------
actual_user_ratings <- edx_train %>% left_join(actual_movie_ratings, by='movieId') %>%
group_by(userId) %>% summarise(b_y = mean(rating - mu_hat_train - b_i))

predicted_user_ratings<-  edx_test %>% left_join(actual_movie_ratings, by= "movieId") %>% left_join(actual_user_ratings, by= "userId") %>% mutate(pred = mu_hat_train + b_i + b_y) %>% .$pred

predicted_user_ratings<- as.numeric(predicted_user_ratings)

rmse_3 <- RMSE(edx_test$rating, predicted_user_ratings)

reg_3 <- data.frame(Approach = "Regression Model with Movie and User Effects", RMSE = rmse_3)

reg_3 %>% knitr::kable()


## ---------------------------------------------------------------------------------------
lambda_list<- seq(0,10,0.2)
lambda_rmses <- sapply(lambda_list, function(l){
  
b_i <- edx_train %>%  group_by(movieId) %>% summarise(b_i = sum(rating - mu_hat_train)/(n()+l))

b_y <- edx_train %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarise(b_y = sum(rating - b_i - mu_hat_train)/(n()+l))
   
predicted_ratings <- edx_test %>% left_join(b_i, by = "movieId") %>% left_join(b_y, by = "userId") %>% mutate(pred = mu_hat_train + b_i + b_y) %>% .$pred
   
return(RMSE(edx_test$rating, predicted_ratings))

})


## ---------------------------------------------------------------------------------------
lambda_plot<- plot(lambda_list, lambda_rmses)
lambda<- lambda_list[which.min(lambda_rmses)]
lambda


## ---------------------------------------------------------------------------------------
reg_movie_ratings <- edx_train %>% group_by(movieId) %>% summarise(b_i = sum(rating - mu_hat_train)/(n()+lambda), n_i = n())

user_avgs_reg <- edx_train %>% left_join(reg_movie_ratings, by="movieId") %>% group_by(userId) %>% summarise(b_y = sum(rating - mu_hat_train - b_i)/(n()+lambda), n_y = n())

reg_user_ratings <- edx_train %>% left_join(reg_movie_ratings, by="movieId") %>% group_by(userId) %>% summarise(b_y = sum(rating - mu_hat_train - b_i)/(n()+lambda), n_y = n())

reg_predicted_ratings <- edx_test %>% left_join(reg_movie_ratings, by="movieId") %>% left_join(reg_user_ratings, by="userId") %>% mutate(pred = mu_hat_train + b_i + b_y) %>%  .$pred

rmse_4 <- RMSE(edx_test$rating,reg_predicted_ratings)

reg_4 <- data_frame(Approach="Regression Model with Effects and Regularisation", RMSE = rmse_4 )

reg_4 %>% knitr::kable()


## ---------------------------------------------------------------------------------------
reg_movie_ratings_val <- edx %>% group_by(movieId) %>% summarise(b_i = sum(rating - mu_hat_train)/(n()+lambda), n_i = n())

user_avgs_reg_val <- edx %>% left_join(reg_movie_ratings_val, by="movieId") %>% group_by(userId) %>% summarise(b_y = sum(rating - mu_hat_train - b_i)/(n()+lambda), n_y = n())

reg_user_ratings_val <- edx %>% left_join(reg_movie_ratings_val, by="movieId") %>% group_by(userId) %>% summarise(b_y = sum(rating - mu_hat_train - b_i)/(n()+lambda), n_y = n())

reg_predicted_ratings_val <- validation %>% left_join(reg_movie_ratings_val, by="movieId") %>% left_join(reg_user_ratings_val, by="userId") %>% mutate(pred = mu_hat_train + b_i + b_y) %>%  .$pred

rmse_5 <- RMSE(validation$rating,reg_predicted_ratings_val)

reg_5 <- data_frame(Approach="Regression Model with Regularisation on Validation Set", RMSE = rmse_5 )

reg_5 %>% knitr::kable()


## ---------------------------------------------------------------------------------------
total_reg<- data.frame(c("Naive", "Movie Effect", "Movie and User Effects", "Regularisation", "Regularisation on Validation"), c(rmse_1, rmse_2, rmse_3, rmse_4, rmse_5))

colnames(total_reg)<-(c("Regression Approach","RMSE"))

total_reg %>% knitr::kable()

