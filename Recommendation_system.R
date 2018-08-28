#############################################################
### 
# Date: 14th Jan , 2017 
# Version : 1.0
# Author : Amit Shyamsukha
# Subject : Beer recommendation system
# History : 
############################################################

library (dplyr)
library(recommenderlab)
library(ggplot2)

beer_data <- read.csv("beer_data.csv" , stringsAsFactors = FALSE)
str(beer_data)
nrow(beer_data)                                ## Total 475984 rows in dataset 
sapply(beer_data, function(x) sum(is.na(x)))   ## There is no missing value in any of the columns
unique(beer_data$review_overall)               ##  0 rating is there in the dataset 
length(which(duplicated(beer_data)))           ## There are duplicate rows in dataset 
mean(beer_data$review_overall)                 ## Mean rating is 3.81 
sapply(beer_data , function(x) length(unique(toupper(x)))-length(unique(tolower(x))))  ## No case sensitivity issues are there 
length(which(beer_data$review_profilename=='')) ## 100 rows with blank username 

## Remove rows having balnk username and 0 ratings 
beer_data <- filter (beer_data , !beer_data$review_overall == 0  )
beer_data <- filter (beer_data , !beer_data$review_profilename == ''  )

## Remove duplicate rows 
beer_data <- beer_data[ ! duplicated(beer_data), ] 

## Find out review count and mean rating for each beer 
beer_review_summary <- beer_data %>% group_by(beer_beerid)%>% 
         summarise(review_cnt = n(), mean_rating = mean(review_overall))

hist(beer_review_summary$review_cnt , breaks=50)

##  Define a function for bucketing and find out number of reviews 
##  and mean rating for each bucket 

diff_bucket_summary <- function( start_interval, end_interval, dataset) {
  df <- data.frame()
  for (p in start_interval : end_interval) {
       dataset$review_cnt_bucket <- ifelse(dataset$review_cnt < p , paste("<" ,p) , paste(">" ,p))
       dataset_summary <- dataset %>% group_by (review_cnt_bucket ) %>% summarise(mean_bucket_rating = mean (mean_rating ))
       df <- rbind(df, dataset_summary)
     }
       return (df)
  }

## Create different buckets of size  10 to 50 and find out mean rating for each buckets
mean_bukcet_rating  <-diff_bucket_summary(10,50,beer_review_summary)

## Find out bucket for which average rating is greather than mean rating of dataset 
mean_bukcet_rating[which(mean_bukcet_rating$mean_bucket_rating > mean(beer_data$review_overall) ),]

## For beers having at least 46 rating , their average rating is greather than mean rating 
## Of entire dataset. SO we have choosen beers having at least 46 reviews. 

selected_beer_id <- beer_review_summary[which(beer_review_summary$review_cnt >45),"beer_beerid"]

## Create a new data set with beers having at least 46 ratings
beer_dataset <- merge(beer_data  , selected_beer_id , by = "beer_beerid" )

## Reorder the columns to have first user , then beer and then rating 
beer_dataset <- beer_dataset[ , c (2,1,3)]

nrow(beer_dataset)     ###303957
str(beer_dataset)

## Find for rows having same user and item but  multiple ratings 
beer_mul_rating <-beer_dataset%>%group_by(review_profilename , beer_beerid) %>% summarise(review_cnt = n())
length(which(beer_mul_rating$review_cnt>1)) ## 635 rows 

merge_beer_data <- merge(beer_dataset ,beer_mul_rating , by = c ( "review_profilename","beer_beerid" ) )
final_beer_dataset <- filter ( merge_beer_data, review_cnt==1)

## convert the data into realRatingMatrix
r <- as(final_beer_dataset, "realRatingMatrix")
class (r)

dimnames(r)
rowCounts(r)
colCounts(r)
rowMeans(r)

##How similar are the first ten users are with each other
 
similar_users <- similarity(r[1:10, ],
                            method = "cosine",
                            which = "users")

#Similarity matrix
as.matrix(similar_users)

#Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")

#Inference 
#Example : Users 05Harley  and 0110x011 are similar. 100floods and 0110x011 are similar. 


#How similar are the first five items are with each other

similar_items <- similarity(r[,1:10 ],
                            method = "cosine",
                            which = "items")
as.matrix(similar_items)

#Visualise similarity matrix
image(as.matrix(similar_items), main = "Item similarity")

## Example : 5 and 14 are similar. 14 and 19 are similar. 

## Unique values of rating 
unique(getRatings(r))   ###4.0 3.5 4.5 3.0 2.5 1.0 5.0 2.0 1.5

##Visualize the ratins 
hist(getRatings(r), breaks="FD")

##  average beer ratings
qplot(colMeans(r), binwidth = .1, 
      main = "Mean rating of beer", 
      xlab = "Rating ", 
      ylab = "# of beers rated")


## Count of beer rating 
qplot(colCounts (r), binwidth = 100 , 
      main = "Count of beer rating", 
      xlab = "# of rating", 
      ylab = "# of beer")

##  Average of user ratings
qplot(rowMeans(r), binwidth = .1)

##  Count of user ratings
qplot(rowCounts(r), binwidth = 20)

#
avg_rating_to_beers <- mean(colMeans(r))     ##  The average number of ratings given to the beers 3.821081
avg_rating_by_users <- mean(rowMeans(r))     ##  The average number of ratings given by the users 3.950247


### Start building Recommendation Models

## Cross validation evalution scheme

scheme <- evaluationScheme(r[1:10000], method="cross", k=4, given=-1,
                            goodRating = 4 )
scheme

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)

# run algorithms, predict next n movies

results <- evaluate(scheme, algorithms, 
                     n=c(1,3,5,10,15,20))

results
avg(results)

# Draw ROC curve
plot(results, annotate=TRUE)  ## UBCF looks better than IBCF

## Split methodology
scheme <- evaluationScheme(r, method = "split", train = .6,
                           k = 1, given = -1, goodRating = 4)
?evaluationScheme

scheme

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)

# run algorithms, predict next n movies
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results)

# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")


Rec.model<-Recommender(r, method = "UBCF")

recommended.items.cokes <- predict(Rec.model, r["cokes",], n=5)
as(recommended.items.cokes, "list")  ##"42533" "7971"  "1867"  "226"   "1005" 


recommended.items.genog <- predict(Rec.model, r["genog",], n=5)
as(recommended.items.genog, "list")  ##  "7971"  "1372"  "1717"  "276"   "54904"

recommended.items.giblet <- predict(Rec.model, r["giblet",], n=5)
as(recommended.items.giblet, "list") ### "7971"  "21690" "22227" "10325" "34420" 

