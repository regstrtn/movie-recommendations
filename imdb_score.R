library(dplyr)

#Sorry I deleted the import file line. Please enter that again :P

#mv = mv[!((mv$imbd_score == "") | mv$imdb_score == NULL)]

training_data = mv[1:4000,-c(17,10,22)]
test_data = data.frame(mv[4001:nrow(mv),-c(17,10,22)]) 

#formula1 = training_data$imdb_score ~ training_data$color + training_data$director_facebook_likes + training_data$actor_3_facebook_likes + training_data$actor_1_facebook_likes + training_data$gross + training_data$num_voted_users + training_data$cast_total_facebook_likes  + training_data$budget + training_data$actor_2_facebook_likes + training_data$aspect_ratio + training_data$movie_facebook_likes
#The above way of writing the formula is not correct. Which is why getting that variables had 4000 rows error

formula1 = imdb_score ~ color + director_facebook_likes + actor_3_facebook_likes + actor_1_facebook_likes + gross + num_voted_users +cast_total_facebook_likes  + budget + actor_2_facebook_likes + aspect_ratio + movie_facebook_likes
#+ training_data$genres + training_data$content_rating
#model1 = glm(formula = formula1 , data = training_data , family = binomial)
model1  = lm(formula = formula1 , data = training_data)

#p = predict(model,test_data , response = '')

p = predict(model1,newdata =  test_data)
