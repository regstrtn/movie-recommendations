Sys.setenv(https_proxy="http://10.3.100.207:8080")
Sys.setenv(http_proxy="http://10.3.100.207:8080")
library(ggplot2)
library(dplyr)
library(lsa)
library(class)

#Read data
m <- read.csv("~/Downloads/sem1/clab1/a8/clean-file", stringsAsFactors=FALSE)
s = m%>%group_by(color)%>%summarise(n = n(), mn = mean(imdb_score, na.rm = T))
m = unique(m)

#Run the below code to get rid of random shit in movie_title
#tr -d '\302\240' < movie_metadata.csv > clean-file

#Deal with NA values
duration = m%>%group_by(as.factor(genres))%>%summarise(n= n(), mn = mean(duration,na.rm = T))
m$facenumber_in_poster[is.na(m$facenumber_in_poster)] = 1
m$duration[is.na(m$duration)] = 103.0
m = m[m$movie_title!="" ,]
m = m[m$director_name!="",]
m = m[m$language!="",]
m = m[(m$actor_1_name!="" | m$actor_2_name!="" | m$actor_3_name!=""),]
m = m[!is.na(m$title_year),]
m$gross[is.na(m$gross)] = median(m$gross, na.rm = T)
m$budget[is.na(m$budget)] = median(m$budget, na.rm = T)
m$actor_2_facebook_likes[is.na(m$actor_2_facebook_likes)] = 0
m$actor_3_facebook_likes[is.na(m$actor_3_facebook_likes)] = 0
m$num_user_for_reviews[is.na(m$num_user_for_reviews)] = median(m$num_user_for_reviews, na.rm = T)
m$num_critic_for_reviews[is.na(m$num_critic_for_reviews)] = median(m$num_critic_for_reviews, na.rm = T)
#m = m[m$budget<250000001 & m$budget>=10000,]
#m = m[m$imdb_score>2.5,]

#Get first genre
m$newg = sapply(strsplit(m$genres,split = "|",fixed = T), function(x) x[[1]])

#Not required
notrequired = c("color","aspect_ratio","content_rating","genres","director_name","actor_1_name","actor_2_name","actor_3_name","movie_title","plot_keywords","movie_imdb_link","language","country")
mnew = m[,!names(m) %in%  notrequired]

#Normalise all numeric values
for (i in 1:ncol(mnew)) {
  if(is.numeric(mnew[,i])) {
    mn = mean(mnew[,i],na.rm = T)
    sd = sd(mnew[,i], na.rm = T)
    mnew[,i] = (mnew[,i] - mn)/sd
  }
}

#Split into test and train
mtr = mnew[1:4300, ]
mts = mnew[4301:nrow(mnew),]

pg = rep("", nrow(mts))
pg = knn(mtr[,-16], mts[,-16], m$newg[1:4300],k = 47)
#Use knn model

mts$pgenre = pg
#Evaluate model
eq = length(which(mts$newg == pg))
noteq = length(which(mts$newg!= pg))
cat("Prediction accuracy of knn model: ",eq/(noteq+eq))
